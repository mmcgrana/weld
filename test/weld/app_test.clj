(ns weld.app-test
  (:use clj-unit.core
        (weld app routing request self-test-helpers config))
  (:require (weld routing routing-test)))

(defn echo [req]
  req)

(defn raise [req]
  (throw (Exception. "o noes")))

(defn handle [req]
  (assoc req ::handled true))

(def router
  (compiled-router [['weld.app-test/echo  :echo   :get "/echo/:id"]
                    ['weld.app-test/raise :raise  :get "/raise"]
                    ['foo/bar             :foobar :get "/foo/bar"]]))

(def config
  {'weld.app/*logger*      {:test (constantly true) :log identity}
   'weld.app/*handler-sym* 'weld.app-test/handle
   'weld.routing/*router*  router})

(deftest "app: route does not resolve"
  (with-config config
    (assert-throws #"Routed to symbol that does not resolve: foo/bar"
      (app (req-with {:request-method :get :uri "/foo/bar"})))))

(deftest "app: unhandled error"
  (with-config (assoc config 'weld.app/*handler-sym* nil)
    (assert-throws #"o noes"
      (app (req-with {:request-method :get :uri "/raise"})))))

(deftest "app: mihandled error"
  (with-config (assoc config 'weld.app/*handler-sym* 'foo/bar)
    (assert-throws #"Handler symbol does not resolve: foo/bar"
      (app (req-with {:request-method :get :uri "/raise"})))))

(deftest "app: handled error"
  (with-config config
    (let [req-echo (app (req-with {:request-method :get :uri "/raise"}))]
      (assert= :get (request-method req-echo))
      (assert-that (get req-echo ::handled)))))

(deftest "app: normal response"
  (with-config config
    (let [req-echo (app (req-with {:request-method :get :uri "/echo/bat"
                                   :body (str-input-stream "foobar")}))]
      (assert= {:id "bat"} (params req-echo))
      (assert= :get        (request-method req-echo))
      (assert= "foobar"    (body-str req-echo)))))


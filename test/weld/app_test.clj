(ns weld.app-test
  (:use clj-unit.core
        (weld app routing request self-test-helpers config))
  (:require (weld routing routing-test)))

(defn echo [req] req)

(def router
  (compiled-router [['weld.app-test/echo :echo   :get "/echo/:id"]
                    ['foo/bar            :foobar :get "/foo/bar"]]))

(def config
  {'weld.routing/*router* router
   'weld.app/*logger*       {:test (constantly true) :log identity}})

(deftest "app"
  (with-config config
    (assert-throws #"Routed to symbol that does not resolve: foo/bar"
      (app (req-with {:request-method :get :uri "/foo/bar"})))
    (let [req-echo (app (req-with {:request-method :get :uri "/echo/bat"
                                   :body (str-input-stream "foobar")}))]
      (assert= {:id "bat"} (params req-echo))
      (assert= :get        (request-method req-echo))
      (assert= "foobar"    (body-str req-echo)))))


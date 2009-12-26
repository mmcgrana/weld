(ns weld.app-test
  (:use (clj-unit core)
        (weld app routing request self-test-helpers)))

(defn handle [req]
  (assoc req ::handled true))

(defn raise [req]
  (throw (Exception. "o noes")))

(defn rescue [req]
  (assoc req ::rescued true))

(def router
  (compiled-router [['weld.app-test/handle :echo   :get "/handle/:id"]
                    ['weld.app-test/raise  :raise  :get "/raise"]
                    ['weld.app-test/miss   :miss   :get "/miss"]]))

(def handled-app
  (handler {:router router :failsafe 'weld.app-test/rescue}))

(def unhandled-app
  (handler {:router router}))

(def mishandled-app
  (handler {:router router :failsafe 'weld.app-test/miss}))

(deftest "app: normal response"
  (let [resp (handled-app (req-with {:request-method :get :uri "/handle/bat"
                                     :body (str-input-stream "foobar")}))]
    (assert= {:id "bat"} (params resp))
    (assert= :get        (request-method resp))
    (assert= "foobar"    (body-str resp))))

(deftest "app: route does not resolve"
  (assert-throws #"Routed to symbol that does not resolve: weld.app-test/miss"
    (handled-app (req-with {:request-method :get :uri "/miss"}))))

(deftest "app: unhandled error"
  (assert-throws #"o noes"
    (unhandled-app (req-with {:request-method :get :uri "/raise"}))))

(deftest "app: handled error"
  (let [resp (handled-app (req-with {:request-method :get :uri "/raise"}))]
    (assert-that (::rescued resp))))

(deftest "app: mihandled error"
  (assert-throws #"Handler symbol does not resolve: weld.app-test/miss"
    (mishandled-app (req-with {:request-method :get :uri "/raise"}))))

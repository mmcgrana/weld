(ns weld.routing-test
  (:use clj-unit.core weld.routing))

(def show (fn [req] req))

(def routes
  [['weld.routing-test/index          :index     :get  "/"                  ]
   ['weld.routing-test/show           :show      :get  "/show/:slug"        ]
   ['weld.routing-test/page-not-found :not-found :any  "/:path" {:path ".*"}]])

(def test-router (compiled-router "host" routes))

(defmacro deftest-conf
  [name & body]
  `(deftest ~name
     (binding [weld.routing/router test-router]
       ~@body)))

(deftest-conf "recognize"
  (let [[fn-sym params] (recognize :get "/show/foo")]
    (assert= 'weld.routing-test/show fn-sym)
    (assert= :shown ((resolve fn-sym) :shown))
    (assert= {:slug "foo"} params)))

(deftest-conf "path-info"
  (let [info   [:get "/show/foo" {:extra "bar"}]
        params {:slug "foo" :extra "bar"}]
    (assert= info (path-info :show params))))

(deftest-conf "path"
  (let [the-path "/show/foo"
        params   {:slug "foo" :extra "bar"}]
    (assert= the-path (path :show params))))

(deftest-conf "url-info"
  (let [info   [:get "host/show/foo" {:extra "bar"}]
        params {:slug "foo" :extra "bar"}]
    (assert= info (url-info :show params))))

(deftest-conf "url"
  (let [the-path "host/show/foo"
        params   {:slug "foo" :extra "bar"}]
    (assert= the-path (url :show params))))
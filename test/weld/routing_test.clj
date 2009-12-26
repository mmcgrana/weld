(ns weld.routing-test
  (:use (clj-unit core) (weld routing)))

(def routes
  [['index          :index     :get  "/"                       ]
   ['show           :show      :get  "/show/:slug"             ]
   ['page-not-found :not-found :any  "/:path"      {:path ".*"}]])

(def router (compiled-router routes))
(def host "host")

(deftest "recognize"
  (let [[action-sym params] (recognize router :get "/show/foo")]
    (assert= 'show action-sym)
    (assert= {:slug "foo"} params)))

(deftest "path-info"
  (let [info   [:get "/show/foo" {:extra "bar"}]
        params {:slug "foo" :extra "bar"}]
    (assert= info (path-info router :show params))))

(deftest "path"
  (let [the-path "/show/foo"
        params   {:slug "foo" :extra "bar"}]
    (assert= the-path (path router :show params))))

(deftest "url-info"
  (let [info   [:get "host/show/foo" {:extra "bar"}]
        params {:slug "foo" :extra "bar"}]
    (assert= info (url-info router host :show params))))

(deftest "url"
  (let [the-path "host/show/foo"
        params   {:slug "foo" :extra "bar"}]
    (assert= the-path (url router host :show params))))
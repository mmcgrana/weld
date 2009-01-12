(ns weld.routing
  (:use clj-routing.core weld.utils))

(defn compiled-router
  "Returns a router object that can then be used in any of the routing functions
  below. TODO: doc routes format."
  [root routes]
  {:symbolic-recognizer
     (compile-recognizer
       (map (fn [[fn-sym name meth path opts]] [fn-sym meth path opts])
            routes))
   :path-info
     (compile-generator
       (map (fn [[fn-sym name meth path opts]] [name meth path opts])
            routes))
   :root root})

(defn recognize
  "Returns an [qualified-fn-sym params] tuple based on the router, http method,
  and  path."
  [router method path]
  ((:symbolic-recognizer router) method path))

(defn path-info
  "Returns a [method path unused-params] tuple based on the router, action name,
  and params."
  [router name & [params]]
  ((:path-info router) name params))

(defn path
  "Returns a path based on the router, action name, and optional params."
  [router name & [params]]
  (nth (path-info router name params) 1))

(defn absolutize
  "Returns a fully qualifid version of the path based on the root in the 
  router."
  [router path]
  (str (:root router) path))

(defn url-info
  "Returns a [method url unused-params] tuple based on the router, action name, 
  and optional params."
  [router name & [params]]
  (let [path-tuple (path-info router name params)]
    (assoc path-tuple 1 (absolutize router (nth path-tuple 1)))))

(defn url
  "Returns a full url based on the router, action name, and optional params."
  [router name & [params]]
  (absolutize router (path router name params)))

(defmacro defrouting
  "Define convenience routing functions based on the root and routes coll, as
  per compiled-router.
  Defines vars router, path-info, path, url-info, and url, where the latter
  4 are such that (path-info :foo) <=> (ring.routing/path-info router :foo)."
  [root routes]
  `(do
     (def ~'router (weld.routing/compiled-router ~root ~routes))
     (def ~'path-info (partial weld.routing/path-info ~'router))
     (def ~'path      (partial weld.routing/path      ~'router))
     (def ~'url-info  (partial weld.routing/url-info  ~'router))
     (def ~'url       (partial weld.routing/url       ~'router))))
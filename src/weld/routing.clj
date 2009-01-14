(ns weld.routing
  (:use clj-routing.core weld.utils))

(declare *router* *host*)

(defn compiled-router
  "Returns a router object that can in turn be bound to the *router* dynamic var
  in this namespace. The routes argument must be a seq of tuples, where each
  tuple is of the form:
  [qualified-fn-sym name method-keyword path-pattern & [path-options]]."
  [routes]
  {:symbolic-recognizer
     (compile-recognizer
       (map (fn [[fn-sym name meth path opts]] [fn-sym meth path opts])
            routes))
   :path-info
     (compile-generator
       (map (fn [[fn-sym name meth path opts]] [name meth path opts])
            routes))})

(defn recognize
  "Returns an [qualified-fn-sym params] tuple based on the http method, and
  path."
  [method path]
  ((:symbolic-recognizer *router*) method path))

(defn path-info
  "Returns a [method path unused-params] tuple based on the action name, and
  params."
  [name & [params]]
  ((:path-info *router*) name params))

(defn path
  "Returns a path based on the action name, and optional params."
  [name & [params]]
  (nth (path-info name params) 1))

(defn absolutize
  "Returns a fully qualifid version of the path."
  [path]
  (str *host* path))

(defn url-info
  "Returns a [method url unused-params] tuple based on the router, action name, 
  and optional params."
  [name & [params]]
  (let [path-tuple (path-info name params)]
    (assoc path-tuple 1 (absolutize (nth path-tuple 1)))))

(defn url
  "Returns a full url based on the router, action name, and optional params."
  [name & [params]]
  (absolutize (path name params)))

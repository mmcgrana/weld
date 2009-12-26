(ns weld.routing
  (:use (clj-routing core)))

(defn compiled-router
  "Returns a router object that can in turn be used as an argument to other
  routing functions. The routes argument must be a seq of tuples, where each
  tuple is of the form:
  [action-sym name http-method path-pattern & [path-options]]."
  [routes]
  {:recognize
     (compiled-recognizer
       (map (fn [[action-sym name meth path opts]] [action-sym meth path opts])
            routes))
   :path-info
     (compiled-generator
       (map (fn [[action-sym name meth path opts]] [name meth path opts])
            routes))})

(defn recognize
  "Returns a [action-sym params] tuple based on the http method and path."
  [router method path]
  ((:recognize router) method path))

(defn path-info
  "Returns a [method path unused-params] tuple based on the action name and
  optional params."
  [router name & [params]]
  ((:path-info router) name params))

(defn path
  "Returns a path based on the action name, and optional params."
  [router name & [params]]
  (nth (path-info router name params) 1))

(defn absolutize
  "Returns a fully qualifid version of the path."
  [host path]
  (str host path))

(defn url-info
  "Returns a [method url unused-params] tuple based on the action name and 
  optional params."
  [router host name & [params]]
  (let [path-tuple (path-info router name params)]
    (assoc path-tuple 1 (absolutize host (nth path-tuple 1)))))

(defn url
  "Returns a full url based on the router, action name, and optional params."
  [router host name & [params]]
  (absolutize host (path router name params)))

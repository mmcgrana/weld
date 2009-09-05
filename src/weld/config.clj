(ns weld.config
  (use clojure.contrib.except))

(defmacro binding*
  "Like binding, but takes a runtime map of vars to values."
  [vars-to-vals & body]
  `(do
     (clojure.lang.Var/pushThreadBindings ~vars-to-vals)
     (try
       ~@body
       (finally (clojure.lang.Var/popThreadBindings)))))

(defn var-bindings
  "Returns a map of vars to vals corresponding to the given map of qualified
  symbols to vals."
  [config]
  (into {} (for [[sym val] config]
             [(or (resolve sym)
                  (throwf "The symbol %s did not resolve to a Var" sym)) val])))

(defn wrap-config [config app]
  "Wraps an app such that the given config values are thread-locally bound for
  the duration of each invocation of the app."
  (let [var-binds (var-bindings config)]
    (fn [req]
      (binding* var-binds (app req)))))

(defmacro with-config
  "Executes body in the context of thread-locally binding the given configs."
  [config-form & body]
  `(binding* (var-bindings ~config-form)
     ~@body))

(defn use-config
  "Atomically alter root values of vars according to the given configs."
  [config]
  (doseq [[var val] (var-bindings config)]
    (.bindRoot var val)))
(ns weld.utils
  (:use clojure.contrib.str-utils))

(defn re-match?
  "Returns true iff the given string contains a match for the given pattern."
  [#^java.util.regex.Pattern pattern string]
  (.find (.matcher pattern string)))

(defn str-cat
  "Concat the given strings into a single string. Like (str-join \"\" strs)."
  [strings]
  (apply str strings))

(defn trim
  "Trims a string, removing leading and trailing whitespace."
  [#^String string]
  (.trim string))

(defmacro get-or
  "Short for (or (get map key) or-form)."
  [map key or-form]
  `(or (get ~map ~key) ~or-form))

; http://paste.lisp.org/display/73551"
(defmacro binding*
  "Like binding, but takes a runtime map of vars to values."
  [vars-to-vals & body]
  `(do
     (clojure.lang.Var/pushThreadBindings ~vars-to-vals)
     (try
       ~@body
       (finally (clojure.lang.Var/popThreadBindings)))))

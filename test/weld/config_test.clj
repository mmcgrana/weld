(ns weld.config-test
  (:use clj-unit.core weld.config))

(def a-var)

(deftest "binding*"
  (assert= :a-var
    (binding* {#'a-var :a-var}
      a-var)))

(def config {'weld.config-test/a-var :a-var})

(deftest "var-bindings"
  (assert= {#'weld.config-test/a-var :a-var} (var-bindings config))
  (assert-throws #"The symbol not-a-var did not resolve to a Var"
    (var-bindings {'not-a-var :not-a-var})))

(deftest "wrap-config"
  (let [wrapped (wrap-config config (fn [req] a-var))]
    (assert= :a-var (wrapped :req))))

(deftest "with-config"
  (with-config config
    (assert= :a-var a-var)))

(deftest "use-config"
  (use-config {'weld.config-test/a-var :altered-a-var})
  (assert= a-var :altered-a-var))

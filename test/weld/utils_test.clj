(ns weld.utils-test
  (:use clj-unit.core weld.utils))

(deftest "re-match?"
  (assert-that (re-match? #"foo" "foo"))
  (assert-that (re-match? #"o"   "foo"))
  (assert-not  (re-match? #"bar" "foo")))

(deftest "str-cat"
  (assert= ""       (str-cat (list)))
  (assert= "foobar" (str-cat (list "foo" "bar"))))

(deftest "trim"
  (assert= "foo" (trim " foo "))
  (assert= "foo" (trim " \nfoo\n ")))

(deftest "get-or"
  (assert= :bar (get-or {:foo :bar} :foo (throw (Exception. "fail"))))
  (assert= :bat (get-or {:foo :bar} :biz :bat)))

(deftest "binding*"
  (declare foo bar)
  (assert= [:foo :bar]
    (binding* {#'foo :foo #'bar :bar}
      [foo bar])))

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

(deftest "memoize-by"
  (let [memoized (memoize-by :mem-key :val-key)]
    (let [h1 {:mem-key 1 :val-key :a}
          h2 {:mem-key 1 :val-key :b}
          h3 {:mem-key 2 :val-key :c}]
      (assert= (memoized h1) :a)
      (assert= (memoized h1) :a)
      (assert= (memoized h2) :a)
      (assert= (memoized h3) :c))))

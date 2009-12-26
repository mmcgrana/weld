(ns weld.http-utils-test
  (:use (clj-unit core)
        (weld http-utils)))

(deftest "url-escape, url-unescape: round-trip as expected"
  (let [given "foo123!@#$%^&*(){}[]<>?/"]
    (assert= given (url-unescape (url-escape given)))))

(deftest "base64-encode, base64-decode"
  (let [data    {:foo "bar" }
        encoded (base64-encode (pr-str data))]
    (assert-match #"[a-zA-Z0-9\+\/]" encoded)
    (assert= data (read-string (base64-decode encoded)))))

(def query-parse-cases
  [[""                                    {}]
   ["foo=bar&baz=bat"                     {:foo "bar", :baz "bat"}]
   ["foo=bar&foo=baz"                     {:foo "baz"}]
   ["foo[]=bar&foo[]=baz"                 {:foo ["bar" "baz"]}]
   ["foo[][bar]=1&foo[][bar]=2"           {:foo [{:bar "1"} {:bar "2"}]}]
   ["foo[bar][][baz]=1&foo[bar][][baz]=2" {:foo {:bar [{:baz "1"} {:baz "2"}]}}]
   ["foo[1]=bar&foo[2]=baz"               {:foo {:1 "bar" :2 "baz"}}]
   ["foo[bar][baz]=1&foo[bar][zot]=2&foo[bar][zip]=3&foo[bar][buz]=4" {:foo {:bar {:baz "1" :zot "2" :zip "3" :buz "4"}}}]
   ["foo[bar][][baz]=1&foo[bar][][zot]=2&foo[bar][][zip]=3&foo[bar][][buz]=4" {:foo {:bar [{:baz "1" :zot "2" :zip "3" :buz "4"}]}}]
   ["foo[bar][][baz]=1&foo[bar][][zot]=2&foo[bar][][baz]=3&foo[bar][][zot]=4" {:foo {:bar [{:baz "1" :zot "2"} {:baz "3" :zot "4"}]}}]])

(deftest "query-parse: empty"
  (assert-fn empty? (query-parse nil))
  (assert-fn empty? (query-parse "")))

(deftest "query-parse: non-empty"
  (assert= {:foo "bar" :baz "bat"}
           (query-parse "foo=bar&baz=bat")))

(deftest "query-parse: duplicate keys"
  (assert= {:foo ["biz" "bar" "baz"]}
           (query-parse "foo=biz&foo=bar&foo=baz")))

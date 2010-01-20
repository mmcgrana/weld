(ns weld.request-test
  (:use (clj-unit core)
        (weld request self-test-helpers)
        (clojure.contrib str-utils)
        (clojure.contrib [def :only (defvar-)]))
  (:import (java.io File)))

(deftest "headers"
  (assert= {"foo" "bar"} (headers (req-with {:headers {"foo" "bar"}}))))

(deftest "content-type"
  (assert=
    "application/xml"
    (content-type (req-with {:content-type "application/xml"}))))

(deftest "content-length"
  (assert= 37
    (content-length (req-with {:content-length 37}))))

(deftest "character-encoding"
  (assert= "UTF-8"
    (character-encoding (req-with {:character-encoding "UTF-8"}))))

(deftest "query-string"
  (assert= "foo=bar"
    (query-string (req-with {:query-string "foo=bar"}))))

(deftest "uri"
  (assert= "/foo/bar"
    (uri (req-with {:uri "/foo/bar"}))))

(deftest "query-params"
  (assert= {:foo "bar"}
    (query-params (req-with {:query-string "foo=bar"}))))

(deftest "body-str"
  (assert= "foobar"
    (body-str (req-with {:body (str-input-stream "foobar")}))))

(deftest "form-params"
  (assert= nil (form-params (req-with {:body "foo=bar"})))
  (assert= {:foo "bar"}
    (form-params (req-with {:body (str-input-stream "foo=bar")
                            :content-type "application/x-www-form-urlencoded"}))))

(defvar- upload-content-type
  "multipart/form-data; boundary=----WebKitFormBoundaryAyGUY6aMxOI6UF5s")

(defvar- upload-content-length 188)

(defvar- upload-body (str-input-stream
  "------WebKitFormBoundaryAyGUY6aMxOI6UF5s\r\nContent-Disposition: form-data; name=\"upload\"; filename=\"test.txt\"\r\nContent-Type: text/plain\r\n\r\nfoo\r\n\r\n------WebKitFormBoundaryAyGUY6aMxOI6UF5s--"))

(deftest "multipart-params"
  (let [req       (req-with {:content-type   upload-content-type
                             :content-length upload-content-length
                             :body           upload-body})
        mp-params (multipart-params req)
        upload    (:upload mp-params)]
    (assert-instance String  (:filename upload))
    (assert-instance Number  (:size upload))
    (assert-instance String  (:content-type upload))
    (assert-instance File    (:tempfile upload))))

(deftest "mock-params"
  (assert= {:foo "bar"}
    (mock-params (req-with {:weld.request/mock-params {:foo "bar"}}))))

(deftest "params"
  (assert= {:query "query" :mock "mock" :routing "routing"}
    (params (req-with {:query-string "query=query"
                       :weld.request/mock-params {:mock "mock"}}
                      {:routing "routing"}))))

(deftest "request-method*"
  (assert= :get (request-method* (req-with {:request-method :get}))))

(deftest "request-method"
  (assert= :get  (request-method (req-with {:request-method :get})))
  (assert= :post (request-method (req-with {:request-method :post})))
  (assert= :delete
    (request-method (req-with {:request-method :post
                               :weld.request/mock-params {:_method "delete"}})))
  (assert-throws #"Unrecognized piggyback method :post"
    (request-method (req-with {:request-method :post
                               :weld.request/mock-params {:_method "bar"}})))
  (assert-throws #"Unrecognized :request-method :bar"
    (request-method (req-with {:request-method :bar}))))

(deftest "scheme"
  (assert= :http
    (scheme (req-with {:scheme :http}))))

(deftest "ssl?"
  (assert-not  (ssl? (req-with {:scheme :http})))
  (assert-that (ssl? (req-with {:scheme :https}))))

(deftest "server-port"
  (assert= 80 (server-port (req-with {:server-port 80}))))

(def xfhost-header {"x-forwarded-host" "google.com"})
(def h-header      {"host"             "yahoo.com"})
(def server-attrs  {:server-name "ask.com" :server-port 80})

(deftest "server-host"
  (assert= "google.com"
    (server-host (req-with (assoc server-attrs
                             :headers (merge xfhost-header h-header)))))
  (assert= "yahoo.com"
    (server-host (req-with (assoc server-attrs :headers h-header))))
  (assert= "ask.com"
    (server-host (req-with server-attrs))))

(deftest "full-uri"
  (assert= "https://google.com/foo/bar"
    (full-uri (req-with {:uri "/foo/bar" :scheme :https
                         :headers {"host" "google.com"}}))))

(deftest "user-agent"
  (assert= "browser"
    (user-agent (req-with {:headers {"user-agent" "browser"}}))))

(deftest "ajax?"
  (assert-that
    (ajax? (req-with {:headers {"x-requested-with" "XMLHttpRequest"}})))
  (assert-not
    (ajax? (req-with {}))))

(deftest "remote-ip"
  (assert= "rem.addr.only"
    (remote-ip
      (req-with {:remote-addr "rem.addr.only"})))
  (assert= "www.example.com"
    (remote-ip
      (req-with {:headers {"x-forwarded-for" "www.example.com"}})))
  (assert= "www.example.com"
    (remote-ip
      (req-with {:headers {"x-forwarded-for" "192.168.2.1,127.0.0.1,www.example.com"}})))
  (assert= "the.client.ip"
    (remote-ip
      (req-with {:remote-addr "rem.addr.only"
                 :headers {"client-ip" "the.client.ip"
                           "x-forwarded-for" "www.example.com"}}))))

(deftest "referrer"
  (assert= "site"
    (referrer (req-with {:headers {"http-referer" "site"}}))))

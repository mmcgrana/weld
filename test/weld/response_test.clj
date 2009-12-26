(ns weld.response-test
  (:use (clj-unit core)
        (weld response self-test-helpers)))

(deftest "respond"
  (assert=
    {:status 200,
     :body "hello",
     :headers
       {"Content-Type" "text/html",
        "Cache-Control" "private, max-age=0, must-revalidate"}}
    (respond "hello"))
  (assert=
    {:status 404,
     :body "miss",
     :headers
       {"Content-Type" "application/js",
        "Cache-Control" "private, max-age=0, must-revalidate"}}
    (respond "miss" {:status 404 :content-type "application/js"})))

(deftest "redirect"
  (assert=
    {:status  302
     :body    "You are being <a href=\"http://google.com\">redirected</a>."
     :headers {"Location" "http://google.com"}}
    (redirect "http://google.com"))
  (assert=
    {:status 301
     :body   "You are being <a href=\"http://google.com\">redirected</a>."
     :headers {"Location" "http://google.com"}}
    (redirect "http://google.com" {:status 301})))

(def afile  (java.io.File. "/foo/bar.txt"))

(deftest "send-file"
  (assert=
    {:status  200
     :body    afile
     :headers
       {"Content-Transfer-Encoding" "binary"
        "Content-Disposition" "attachment; filename=bar.txt"
        "Cache-Control" "private, max-age=0, must-revalidate"}}
    (send-file afile))
  (assert=
    {:status  404
     :body    afile
     :headers
       {"Content-Transfer-Encoding" "binary"
        "Content-Disposition" "inline; filename=custom.txt"
        "Cache-Control" "private, max-age=0, must-revalidate"}}
    (send-file afile
      {:status 404 :filename "custom.txt" :disposition :inline})))

(def astream (str-input-stream "foobar"))

(deftest "send-stream"
  (assert=
    {:status  200
     :body    astream
     :headers
       {"Content-Transfer-Encoding" "binary"
        "Content-Type" "application/octet-stream"
        "Content-Disposition" "attachment"
        "Cache-Control" "private, max-age=0, must-revalidate"}}
    (send-stream astream))
  (assert=
    {:status  404
     :body    astream
     :headers
       {"Content-Transfer-Encoding" "binary"
        "Content-Type" "application/js"
        "Content-Disposition" "inline; filename=custom.txt"
        "Cache-Control" "private, max-age=0, must-revalidate"}}
    (send-stream astream
      {:status 404 :filename "custom.txt" :disposition :inline
       :content-type "application/js"})))
(ns weld.test-helpers
  (:require (weld [http-utils :as http-utils]))
  (:import (java.io File))
  (:use (clj-unit core)))

(defn request
  "Returns the response of app to mock request build according to the method,
  path and options.
  Options: :remote-addr, :params."
  [app [method path] & [opts]]
  (app {:uri                      path
        :request-method           method
        :remote-addr              (:remote-addr opts)
        :weld.request/mock-params (:params opts)}))

(defn upload
  "Returns an upload hash that can be used as a value in the :params map for
  the mock request helper."
  [file content-type filename]
  {:tempfile file     :size (.length #^File file)
   :filename filename :content-type content-type})

(defn assert-status
  "Assert that a response status equals an expected status."
  [expected-status actual-status]
  (assert-truth (= expected-status actual-status)
    (format "Expected status of %s, but got %s"
      expected-status actual-status)))

(defn assert-redirect
  "Assert that a response tuple indicates a redirect to an expected path."
  [expected-path actual-response]
  (let [{:keys [status headers body]} actual-response
         location             (get headers "Location")]
     (assert-truth (and (and (>= status 300) (< status 400))
                       (= expected-path location))
       (format "Expecting redirect status and Location of %s, but got %s and %s."
         expected-path status location))))

(defn assert-content-type
  "Assert that response headers specify an expected content type."
  [expected-type actual-headers]
  (let [actual-type (get actual-headers "Content-Type")]
    (assert-truth (= expected-type actual-type)
      (format "Expecting Content-Type %s, but got %s"
        expected-type actual-type))))

(ns weld.self-test-helpers
  (:use weld.request clojure.set clojure.contrib.except)
  (:import java.io.ByteArrayInputStream))

(defn str-input-stream
  "Returns a ByteArrayInputStream for the given String."
  [string]
  (ByteArrayInputStream. (.getBytes string)))

(def base-req
  {:server-port        80
   :server-name        "localhost"
   :remote-addr        nil
   :uri                "/foo/bar"
   :query-string       ""
   :scheme             "http"
   :request-method     :get
   :headers            {}
   :content-type       nil
   :content-length     nil
   :character-encoding nil
   :body               (str-input-stream "")})

(defn req-with
  "Returns a mock request, comparable to a request that is passed to a
  controller action in a regular Weld app. Attrs are the same as those that come
  into the app from the last piece of wrapping middleware.
  Pass :weld.request/route-params to spoof route params."
  [attrs]
  (let [route-params (get attrs :weld.request/route-params)]
    (assoc-route-params
      (new-request (merge base-req (dissoc attrs :weld.request/route-params)))
      route-params)))

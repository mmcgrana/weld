(ns weld.http-utils
  (:use (clojure.contrib def str-utils except))
  (:import (org.apache.commons.codec.binary Base64)))

(defn url-escape
  "Returns a url-escaped representation of the given String."
  [unescaped]
  (java.net.URLEncoder/encode unescaped "UTF-8"))

(defn url-unescape
  "Returns a url-unescaped representation of the given String."
  [escaped]
  (java.net.URLDecoder/decode escaped "UTF-8"))

(defn base64-encode-bytes
  "Returns a sring of base64 encoded data for the given unencoded bytes."
  [unencoded]
  (String. (Base64/encodeBase64 unencoded)))

(defn base64-encode
  [unencoded]
  "Returns a string of base64 encoded data for the given unencoded string."
  (base64-encode-bytes (.getBytes #^String unencoded)))

(defn base64-decode
  "Returns a string of base64 decoded data for the given encoded string."
  [encoded]
  (String. (Base64/decodeBase64 (.getBytes #^String encoded))))

(defn- pairs-parse
  "Parse pairs of strings into a params map."
  [pairs]
  (let [non-empty (filter second pairs)]
    (reduce
      (fn [params [key val]]
        (let [ekey (keyword (url-unescape key))
              eval (url-unescape val)
              prev (get params ekey)]
          (cond
            (nil? prev)
              (assoc params ekey eval)
            (vector? prev)
              (assoc params ekey (conj prev eval))
            :else
              (assoc params ekey [prev eval]))))
      {}
      non-empty)))

(defn querylike-parse
  "Helper for public facing functions parsing querystring-like values."
  [separator string]
  (if string
    (let [segments  (re-split separator string)
          pairs     (map #(re-split #"=" % 2) segments)]
      (pairs-parse pairs))))

(defn query-parse
  "Returns a params map query string."
  [query-string]
  (querylike-parse #"&\s*" query-string))

(defn query-unparse
  "The opposite of query-parse, converts a params map into a query string."
  [params]
  (str-join "&"
    (map (fn [[key val]]
           (str (url-escape (name key)) "=" (url-escape val)))
          params)))

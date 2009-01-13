(ns weld.request
  (:use (clojure.contrib def str-utils except)
        (weld utils http-utils))
  (:require [clj-time.core :as time])
  (:import (org.apache.commons.fileupload FileUpload RequestContext)
           (org.apache.commons.fileupload.disk DiskFileItemFactory DiskFileItem)
           (org.apache.commons.io IOUtils)
           (java.io InputStream)
           (org.joda.time.format DateTimeFormat))
  (:load "request_cookies" "request_sessions"))

(defvar- multipart-re         #"multipart/form-data")
(defvar- form-url-encoded-re  #"^application/x-www-form-urlencoded")
(defvar- ajax-http-request-re #"(?i)XMLHttpRequest")
(defvar- local-ip-re          #"^(?i)unknown$|^(127|10|172.16|192.168)\.")

(defvar- recognized-nonpiggyback-methods
  #{:get :head :put :delete :options})

(defn headers
  "Returns the raw headers for the request."
  [req]
  (:headers req))

(defn content-type
  "Returns a String for the requests's content type."
  [req]
  (:content-type req))

(defn content-length
  "Returns an Integer for the requests's content length."
  [req]
  (:content-length req))

(defn character-encoding
  "Returns a String for the request's character encoding."
  [req]
  (:character-encoding req))

(defn query-string
  "Returns a String for the request's query string."
  [req]
  (:query-string req))

(defn uri
  "Returns a String for the requests's uri."
  [req]
  (:uri req))

(defn query-params
  "Returns a possible nested map of query params based on the request's query
  string, or nil if there was no query string."
  [req]
  (query-parse (query-string req)))

(defn- get-delayed
  "Returns the forced delayed value corresponding to the given key, or raises
  if the key is missing."
  [req key]
  (if (contains? req key)
    (force (get req key))
    (throwf (str "missing " key ", only had " (pr-str (keys req))))))

(defn body-str
  "Returns a single String of the raw request body, decoding according to the
  requests character encoding."
  [req]
  (get-delayed req ::body-str-delay))

(defn- body-str-once [req]
  (with-open [#^InputStream stream (:body req)]
    (IOUtils/toString stream (character-encoding req))))

(defn form-params
  "Returs a hash of params described by the request's post body if the 
  content-type indicates a form url encoded request, nil otherwise."
  [req]
  (if-let [ctype (content-type req)]
    (if (re-match? form-url-encoded-re ctype)
      (query-parse (body-str req)))))

(defvar- disk-file-item-factory
  (doto (DiskFileItemFactory.)
    (.setSizeThreshold -1)
    (.setFileCleaningTracker nil))
  "Multipart parsing handler. Saves all multipart param values as tempfiles,
  regardless of size.")

(defn multipart-params
  "Returns a hash of multipart params if the content-type indicates a multipart
  request, nil otherwise."
  [req]
  (get-delayed req ::multipart-params-delay))

(defn- multipart-params-once [req]
  (if-let [ctype (content-type req)]
    (if (re-match? multipart-re ctype)
      (let [upload  (FileUpload. disk-file-item-factory)
            context (proxy [RequestContext] []
                      (getContentType       [] (content-type req))
                      (getContentLength     [] (content-length req))
                      (getCharacterEncoding [] (character-encoding req))
                      (getInputStream       [] (:body req)))
            items   (.parseRequest upload context)
            pairs   (map
                      (fn [#^DiskFileItem item]
                        [(.getFieldName item)
                         (if (.isFormField item)
                           (.getString item)
                           ; need first pair to prevent premature tempfile GC
                           {:disk-file-item item
                            :filename       (.getName item)
                            :size           (.getSize item)
                            :content-type   (.getContentType item)
                            :tempfile       (.getStoreLocation item)})])
                      items)]
      (pairs-parse pairs)))))

(defn mock-params
  "Returns a hash of mock params given directly in the req, if any.
  Used for testing."
  [req]
  (::mock-params req))

(defn params*
  "Returns all params except for those determined from the route."
  [req]
  (get-delayed req ::params*-delay))

(defn- params*-once [req]
  (merge (query-params     req)
         (form-params      req)
         (multipart-params req)
         (mock-params      req)))

(defn route-params
  "Returns the params derived from the uri of the request."
  [req]
  (::route-params req))

(defn params
  "Returns params, including those determined from the route.
  If a single arg is given, an req, returns all such params.
  If additional args are given, they are used to get-in these params"
  ([req]
    (get-delayed req ::params-delay))
  ([req & args]
   (get-in (params req) args)))

(defn- params-once [req]
  (merge (params* req) (route-params req)))

(defn request-method*
  "Returns the literal request method indicated in the reqest, before taking
  into account piggybacking."
  [req]
  (:request-method req))

(defn request-method
  "Returns a Keyword indicating the method for the request, either as literally
  indicated by the request or if available by the _method piggyback param.
  Throws if no valid method is recognized."
  [req]
  (let [r-method (request-method* req)]
    (cond
      (recognized-nonpiggyback-methods r-method)
        r-method
      (= :post r-method)
        (if-let [p-method (:_method (params* req))]
          (or (recognized-nonpiggyback-methods (keyword p-method))
              (throwf "Unrecognized piggyback method %s" r-method))
          :post)
      :else
        (throwf "Unrecognized :request-method %s" r-method))))

(defn scheme
  "Returns a Keyword inidcating the scheme for the request, like :http."
  [req]
  (:scheme req))

(defn ssl?
  "Returns tree iff the request was submitted made over ssl."
  [req]
  (= "https" (scheme req)))

(defn server-port
  "Returns a Integer for the server port."
  [req]
  (:server-port req))

(defn server-host
  "Returns a String for the full hostname, excluding the port."
  [req]
  (let [hdrs (headers req)]
    (or (get hdrs "x-forwarded-host")
        (get hdrs "host")
        (:server-name req))))

(defn full-uri
  "Returns a String for the full request uri, including the protocol and host
  but excluding the port."
  [req]
  (str (scheme req) "://" (server-host req) (uri req)))

(defn user-agent
  "Returns a String for the http user agent."
  [req]
  (get-in req [:headers "user-agent"]))

(defn ajax?
  "Returns true if the given request was an AJAX request."
  [req]
  (if-let [xrw (get-in req [:headers "x-requested-with"])]
    (re-match? ajax-http-request-re xrw)))

(defn remote-ip
  "Returns a String representing our best guess for the IP of the requesting 
  user."
  [req]
  (let [headers (:headers req)]
    (or (get headers "client-ip")
        (if-let [forwarded (get headers "x-forwarded-for")]
          (let [all-ips       (re-split #"," forwarded)
                remote-ips (remove #(re-match? local-ip-re %) all-ips)]
            (if (not (empty? remote-ips)) (trim (first remote-ips)))))
        (:remote-addr req))))

(defn referrer
  "Returns a String for the http refer(r)er"
  [req]
  (get-in req [:headers "http-referer"]))

(defn init
  "Returns a prepared request based on the given raw reqest, where the
  preparations enable the request to correctly handle memoization."
  [req]
  (let [req+   (assoc req   ::body-str-delay         (delay (body-str-once req)))
        req++  (assoc req+  ::multipart-params-delay (delay (multipart-params-once req+)))
        req+++ (assoc req++ ::params*-delay          (delay (params*-once req++)))]
    req+++))

(defn assoc-route-params
  "Returns a new request object like the corresponding to the given one but 
  including the given route-params."
  [req route-params]
  (let [req+  (assoc req  ::route-params route-params)
        req++ (assoc req+ ::params-delay (delay (params-once req+)))]
    req++))

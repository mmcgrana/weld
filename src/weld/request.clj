(ns weld.request
  (:use (clojure.contrib def str-utils except))
  (:require [weld.http-utils :as http-utils])
  (:import (org.apache.commons.fileupload FileUpload RequestContext)
           (org.apache.commons.fileupload.disk DiskFileItemFactory DiskFileItem)
           (org.apache.commons.io IOUtils)
           (java.io InputStream)))

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

(defn body-str
  "Returns a single String of the raw request body, decoding according to the
  requests character encoding. Should only be called once."
  [req]
  (with-open [#^InputStream stream (:body req)]
    (IOUtils/toString stream #^String (character-encoding req))))

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
  (::query-params req))

(defn- parse-query-params [req]
  (http-utils/query-parse (query-string req)))

(defn form-params
  "Returs a hash of params described by the request's post body if the
  content-type indicates a form url encoded request, nil otherwise."
  [req]
  (::form-params req))

(defn- parse-form-params [req]
  (if-let [ctype (content-type req)]
    (if (re-find form-url-encoded-re ctype)
      (http-utils/query-parse (body-str req)))))

(defn multipart-params
  "Returns a hash of multipart params if the content-type indicates a multipart
  request, nil otherwise."
  [req]
  (::multipart-params req))

(defvar- disk-file-item-factory
  (doto (DiskFileItemFactory.)
    (.setSizeThreshold -1)
    (.setFileCleaningTracker nil)))

(defn- parse-multipart-params [req]
  (if-let [ctype (content-type req)]
    (if (re-find multipart-re ctype)
      (let [upload  (FileUpload. disk-file-item-factory)
            context (proxy [RequestContext] []
                      (getContentType       [] (content-type req))
                      (getContentLength     [] (content-length req))
                      (getCharacterEncoding [] (character-encoding req))
                      (getInputStream       [] (:body req)))
            items   (.parseRequest upload context)
            pairs   (map
                      (fn [#^DiskFileItem item]
                        [(keyword (.getFieldName item))
                         (if (.isFormField item)
                           (.getString item)
                           ; need to keep handle on item to prevent tempfile GC
                           (with-meta
                             {:filename       (.getName item)
                              :size           (.getSize item)
                              :content-type   (.getContentType item)
                              :tempfile       (.getStoreLocation item)}
                             {:disk-file-item item}))])
                      items)]
      (into {} pairs)))))

(defn mock-params
  "Returns a hash of mock params given directly in the req, if any.
  Used for testing."
  [req]
  (::mock-params req))

(defn route-params
  "Returns the params derived from the uri of the request."
  [req]
  (::route-params req))

(defn params
  "Returns params, including those determined from the route.
  If a single arg is given, a req, returns all such params.
  If additional args are given, they are used to get-in these params"
  [req]
  (::params req))

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
        (if-let [p-method (:_method (::base-params req))]
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
  (= :https (scheme req)))

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
  (str (name (scheme req)) "://" (server-host req) (uri req)))

(defn user-agent
  "Returns a String for the http user agent."
  [req]
  (get-in req [:headers "user-agent"]))

(defn ajax?
  "Returns true if the given request was an AJAX request."
  [req]
  (if-let [xrw (get-in req [:headers "x-requested-with"])]
    (re-find ajax-http-request-re xrw)))

(defn remote-ip
  "Returns a String representing our best guess for the IP of the requesting
  user."
  [req]
  (let [headers (:headers req)]
    (or (get headers "client-ip")
        (if-let [forwarded (get headers "x-forwarded-for")]
          (let [all-ips       (re-split #"," forwarded)
                remote-ips (remove #(re-find local-ip-re %) all-ips)]
            (if (not (empty? remote-ips)) (.trim #^String (first remote-ips)))))
        (:remote-addr req))))

(defn referrer
  "Returns a String for the http refer(r)er"
  [req]
  (get-in req [:headers "http-referer"]))

(defn assoc-base-params
  "Returns a request augmented with keys for query-, form-, and multipart-
  params, based on the headers, query string, and body of the request, as well
  as a merged map for these three."
  [req]
  (let [q-params (parse-query-params req)
        f-params (parse-form-params req)
        m-params (parse-multipart-params req)]
    (-> req
      (assoc ::query-params q-params)
      (assoc ::form-params f-params)
      (assoc ::multipart-params m-params)
      (assoc ::base-params
        (merge q-params f-params m-params (::mock-params req))))))

(defn assoc-route-params
  "Returns a request with a key added for the given route params, as well as
  a merged map for all of the param types."
  [req route-params]
  (-> req
    (assoc ::route-params route-params)
    (assoc ::params (merge (::base-params req) route-params))))

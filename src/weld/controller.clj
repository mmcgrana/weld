(ns weld.controller
  (:import (org.apache.commons.io FilenameUtils)))

(defn respond
  "Most general function for returning a body-containing Ring response.
  Options:
    :status, defaults to 200
    :content-type, defaults to \"text/html\""
  [body & [opts]]
  (let [status       (or (get opts :status) 200)
        content-type (or (get opts :content-type) "text/html")]
    {:status status
     :body   body
     :headers
       {"Cache-Control" "private, max-age=0, must-revalidate"
        "Content-Type" content-type}}))

(defn redirect
  "Returns a Ring response for a redirect. 
  Options:
    :status, defaults to 302"
  [url & [opts]]
  (let [status (or (get opts :status) 302)]
    {:status  status
     :body    (str "You are being <a href=\"" url "\">redirected</a>.")
     :headers {"Location" url}}))

(defn send-stream
  "Returns a Ring resonse for the given input stream.
  Options:
    :status, defaults to 200
    :filename, no default value
    :disposition, :inline or :attachment, defaults to the latter
    :content-type, defaults to \"application/octet-stream\""
  [stream & [opts]]
  (let [status       (or (get opts :status) 200)
        filename     (get opts :filename)
        disposition  (or (get opts :disposition) :attachment)
        content-type (or (get opts :content-type) "application/octet-stream")]
    {:status status
     :body   stream
     :headers
       {"Cache-Control" "private, max-age=0, must-revalidate"
        "Content-Transfer-Encoding" "binary"
        "Content-Type" content-type
        "Content-Disposition"
          (str (name disposition)
            (if filename (str "; filename=" filename)))}}))

(defn send-file
  "Returns a response Ring response for the given file.
  Options:
    :status, defaults to 200
    :filename, defaults to the name of the given file
    :disposition, :inline or as an :attachment, defaults to :atttachment
  Note that content type and length for File reponses can be set by Ring
  middleware."
  [file & [opts]]
  (let [status      (or (get opts :status) 200)
        filename    (or (get opts :filename)
                        (FilenameUtils/getName (.getPath file)))
        disposition (or (get opts :disposition) :attachment)]
    {:status  status
     :body file
     :headers
       {"Cache-Control" "private, max-age=0, must-revalidate"
        "Content-Transfer-Encoding" "binary"
        "Content-Disposition"
          (str (name disposition) "; filename=" filename)}}))

(in-ns 'weld.request)

(declare *session-cookie-key* *session-secret-key*)

(defn hmac [data]
  "Returns a base64 encoded mac for the given string data, according to the
  *session-secret-key*"
  (let [spec  (SecretKeySpec. (.getBytes *session-secret-key*) "HmacSHA256")
        mac   (doto (javax.crypto.Mac/getInstance "HmacSHA256") (.init spec))
        bytes (.doFinal mac (.getBytes data))]
    (base64-encode-bytes bytes)))

(defn marshal
  "Returns a the session hash data marshaled into a base64 string."
  [sess]
  (base64-encode (pr-str sess)))

(defn unmarshal
  "Returns the session hash data from the given base64 string."
  [marshaled]
  (read-string (base64-decode marshaled)))

(defn load-session
  "Returns the session hash data contained in the cookies of the req, if such
  data is present, or nil otherwise."
  [req]
  (if-let [cookie-data (cookies req *session-cookie-key*)]
    (let [[marshaled digest] (re-split #"--" cookie-data)]
      (if (= digest (hmac marshaled))
        (unmarshal marshaled)))))

(defn dump-session
  "Returns a cookie value that can be set to persist the given session on a
  client."
  [sess]
  (let [marshaled   (marshal sess)
        cookie-data (str marshaled "--" (hmac marshaled))]
    (if (> (.length cookie-data) 4000)
      (throwf "Session exceeds 4k.")
      (cookie-str *session-cookie-key* cookie-data))))

(defn session
  "Returns session data extracted from the req. If only the req is given as
  an argument, returns the complete session hash. Otherwise uses the addional
  args to get-in the session."
  ([req]
   (if-let [loaded (load-session req)]
     (with-meta loaded
       {:had-flash? (contains? loaded :flash)})))
  ([req arg]
   (get (session req) arg)))

(defn write-session
  "Augment a response to include a cookie that will persist the given session."
  [sess resp]
  (let [unflashed (if (get ^sess :had-flash?)
                    (dissoc sess :flash)
                    sess)]
    (conj-cookie resp (dump-session unflashed))))

(defn reset-session
  "Send a blank session cookie to the client, thereby resetting the session
  state."
  [resp]
  (write-session {} resp))

(defmacro with-session
  "Helper macro for evaluating a body in the context of a sesion extracted
  from an request."
  [[bind-sess req-form] & body]
  `(let [~bind-sess (session ~req-form)]
     ~@body))

(defn flash-session
  "Like write-session, but stores an addional 'flash' message that will persist
  only through the next request/response cycle. Note that if you flash a session
  the desired single-cyle durability of the message will only occur if the 
  client is resent the (faded) session on the subsequent request."
  [sess message resp]
  (write-session (assoc sess :flash message)
    resp))

(defn flash-request
  "Like (with-session [sess req] (flash-session sess message @body)).
  Use only if the session is not otherwise being written to in this request
  (use flash-session in that case)."
  [req message resp]
  (with-session [sess req]
    (flash-session sess message resp)))

(defmacro with-fading-session
  "In cases in which a session may be flashed an therefore needs to be resent
  to the client to prevent the flash from persisting into subsequent requests,
  you can use this declarative helper macro to both read the session (which
  you will need to read flash values) and to write-session on the generated
  response."
  [[sess-bind req] & body]
  `(with-session [~sess-bind ~req]
     (write-session ~sess-bind (do ~@body))))

(defn flash
  "Reads flash data from a session. If only the sess argument is given, returns
   all the flash data; if an additional arg is given, uses it to key into the
   session data, which must in that case be an associative data structure."
  ([sess]
   (:flash sess))
  ([sess arg]
   (get (flash sess) arg)))

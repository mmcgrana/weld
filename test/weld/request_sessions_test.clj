(in-ns 'weld.request-test)

(defn response-cookies-data
  [resp]
  (get-in resp [:headers "Set-Cookie"]))

(defn req-with-cookies-data
  [cookies-data]
  (req-with {:headers {"cookie" (str-join "; " cookies-data)}}))

(defn req-from-response
  [resp]
  (req-with-cookies-data (response-cookies-data resp)))

(def blank-response
  {:status :s :headers {} :body :s})

(defn cookied-req []
  (req-from-response (write-session {:foo "bar"} blank-response)))

(defmacro deftest-conf
  [name & body]
  `(deftest ~name
     (binding [weld.request/session-cookie-key :cookie-key
               weld.request/session-secret-key "secret-key"]
       ~@body)))

(deftest-conf "marshal, unmarshal"
  (let [data {:foo "bar"}]
    (assert= data (unmarshal (marshal data)))))

(deftest-conf "dump-session, load-session"
  (let [dumped (dump-session {:foo "bar"})]
    (assert= {:foo "bar"}
      (load-session (req-with {:headers {"cookie" dumped}})))
    (assert-nil
      (load-session (req-with {:headers {"cookies" (str dumped "fake")}})))))

(deftest-conf "write-session, session"
  (assert= {:foo "bar"} (session (cookied-req))))

(deftest-conf "reset-session"
  (assert= (reset-session blank-response) (write-session {} blank-response)))

(deftest-conf "with-session"
  (let [reg-sess (session (cookied-req))]
    (with-session [mac-sess (cookied-req)]
      (assert= reg-sess mac-sess))))

(deftest-conf "session with flash"
  (let [sess      (session (cookied-req))
        auth-sess (assoc sess :session-key :session-value)
        auth-resp (write-session auth-sess blank-response)
        auth-req  (req-from-response auth-resp)]
    (assert= :session-value (session auth-req :session-key))
    (with-session [auth-sess auth-req]
      (let [flash-resp (flash-session auth-sess {:success "!"} blank-response)
            flash-request   (req-from-response flash-resp)]
        (assert= :session-value (session flash-request  :session-key))
        (assert= {:success "!"} (flash (session flash-request )))
        (assert= "!" (flash (session flash-request ) :success))
        (let [unflashed-resp (with-fading-session [fading-sess flash-request ]
                                blank-response)
              unflashed-req  (req-from-response unflashed-resp)]
          (assert= :session-value (session unflashed-req :session-key))
          (assert-nil (flash (session unflashed-req))))))))

(deftest-conf "flash-request "
  (assert=
    (with-session [sess (cookied-req)]
      (flash-session sess :flash-message blank-response))
    (flash-request  (cookied-req) :flash-message blank-response)))

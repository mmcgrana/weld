(ns weld.app
  (:use (weld request routing utils) clj-log.core))

(def *logger* nil)

(defmacro maybe-log
  "Helper for logging around the request/response cycle."
  [msg-form]
  `(if-let [logger# *logger*]
     (if ((:test logger#) :info) ((:log logger#) ~msg-form))))

(defn request-msg [req]
  (str "request: " (.toUpperCase (name (request-method* req))) " "
       (full-uri req)))

(defn routing-msg [qual-fn-sym]
  (str "routing: " (pr-str qual-fn-sym)))

(defn params-msg [req]
  (str "params: " (pr-str (params req))))

(defn response-msg [resp start]
  (let [status (:status resp)]
    (str "response: (" (- (System/currentTimeMillis) start) " msecs) " status
         (if (or (= status 301) (= status 302))
           (str " => " (get-in resp [:headers "Location"]))) "\n")))

(defn new-app
  "Returns an app paramaterized by the given router, as compiled by
  weld.routing/compiled-router, as well as any additional configs, which must
  be a map of vars to values corresponding to bindings to make around each
  request."
  [config]
  (fn [req]
    (binding* config
      (maybe-log (request-msg req))
      (let [start (System/currentTimeMillis)
            req+ (new-request req)]
        (let [method                 (request-method req+)
              uri                    (uri req+)
              [qual-fn-sym r-params] (recognize method uri)
              req++                  (assoc-route-params req+ r-params)
              action-fn              (resolve qual-fn-sym)]
          (maybe-log (routing-msg qual-fn-sym))
          (maybe-log (params-msg req++))
          (let [resp (action-fn req++)]
            (maybe-log (response-msg resp start))
            resp))))))

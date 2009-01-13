(ns weld.app
  (:use weld.request weld.routing clj-log.core))

(defmacro maybe-log
  "Helper for logging around the request/response cycle."
  [logger-sym msg-form]
  `(if (and ~logger-sym ((:test ~logger-sym) :info))
     ((:log ~logger-sym) ~msg-form)))

(defn request-msg [req]
  (str "request: " (.toUpperCase (name (request-method* req))) " "
       (full-uri req)))

(defn routing-msg [qual-fn-sym]
  (str "routing: " (pr-str qual-fn-sym)))

(defn params-msg [req]
  (str "params: " (pr-str (params req))))

(defn response-msg [resp start]
  (str "response: (" (- (System/currentTimeMillis) start) " msecs) "
       (:status resp) "\n"))

(defn new-app
  "Returns an app paramaterized by the given router, as compiled by
  weld.routing/compiled-router."
  [router & [logger]]
  (fn [req]
    (maybe-log logger (request-msg req))
    (let [start (System/currentTimeMillis)
          req+ (new-request req)]
      (let [method                 (request-method req+)
            uri                    (uri req+)
            [qual-fn-sym r-params] (recognize router method uri)
            req++                  (assoc-route-params req+ r-params)
            action-fn              (resolve qual-fn-sym)]
        (maybe-log logger (routing-msg qual-fn-sym))
        (maybe-log logger (params-msg req++))
        (let [resp (action-fn req++)]
          (maybe-log logger (response-msg resp start))
          resp)))))

(ns weld.app
  (:use weld.request weld.routing))

(defmacro log
  [logger message-form]
  `(when ~logger
     (.info ~logger ~message-form)))

(defn request-log [req]
  (str "request: " (.toUpperCase (name (request-method req))) " "
       (full-uri req)))

(defn routing-log [qual-fn-sym]
  (str "routing: " (pr-str qual-fn-sym)))

(defn params-log [req]
  (str "params: " (pr-str (params req))))

(defn response-log [resp start]
  (str "response: (" (- (System/currentTimeMillis) start) " msecs) "
       (:status resp) "\n"))

(defn spawn-app
  "Returns an app paramaterized by the given router, as compiled by
  weld.routing/compiled-router."
  [router & [logger]]
  (fn [req]
    (log logger (request-log req))
    (let [start (System/currentTimeMillis)
          req+ (init req)]
      (let [method                 (request-method req+)
            uri                    (uri req+)
            [qual-fn-sym r-params] (recognize router method uri)
            req++                  (assoc-route-params req+ r-params)
            action-fn              (resolve qual-fn-sym)]
        (log logger (routing-log qual-fn-sym))
        (log logger req++)
        (let [resp (action-fn req++)]
          (log logger (response-log resp start))
          resp)))))

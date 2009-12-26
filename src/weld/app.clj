(ns weld.app
  (:use (weld request routing logger) (clojure.contrib except def))
  (:require (clj-stacktrace [repl :as stacktrace])))

(defn- request-msg [req]
  (str "request: " (.toUpperCase (name (request-method* req))) " "
       (full-uri req)))

(defn- routing-msg [action-sym]
  (str "routing: " (pr-str action-sym)))

(defn- params-msg [req]
  (str "params: " (pr-str (params req))))

(defn- response-msg [resp start]
  (let [status (:status resp)]
    (str "response: (" (- (System/currentTimeMillis) start) " msecs) " status
         (if (or (= status 301) (= status 302))
           (str " => " (get-in resp [:headers "Location"]))) "\n")))

(defn- error-msg [e]
  (str "error:" (stacktrace/pst-str e)))

(defn handler
  "Returns a Weld app according to the given configuration.
   The Weld app is a Ring handler."
  [{:keys [router logger failsafe]}]
  (fn [req]
    (log logger :info (request-msg req))
    (let [start (System/currentTimeMillis)
          req1  (assoc-base-params req)]
      (let [method                (request-method req1)
            uri                   (uri req1)
            [action-sym r-params] (recognize router method uri)
            req2                  (assoc-route-params req1 r-params)
            action-fn             (resolve action-sym)]
        (if-not action-fn
          (throwf "Routed to symbol that does not resolve: %s" action-sym)
          (do
            (log logger :info (routing-msg action-sym))
            (log logger :info (params-msg req2))
            (let [resp (try
                         (action-fn req2)
                         (catch Exception e
                           (log logger :error (error-msg e))
                           (if failsafe
                             (if-let [failsafe-fn (resolve failsafe)]
                               (failsafe-fn req2)
                               (throwf "Handler symbol does not resolve: %s" failsafe))
                             (throw e))))]
              (log logger :info (response-msg resp start))
              resp)))))))

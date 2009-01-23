(ns weld.app
  (:use (weld request routing) clojure.contrib.except clj-backtrace.repl))

(declare *logger* *handler-sym*)

(defmacro log
  "Helper for logging around the request/response cycle."
  [level-form msg-form]
  `(if-let [logger# *logger*]
     (if ((:test logger#) ~level-form) ((:log logger#) ~msg-form))))

(defn- request-msg [req]
  (str "request: " (.toUpperCase (name (request-method* req))) " "
       (full-uri req)))

(defn- routing-msg [fn-sym]
  (str "routing: " (pr-str fn-sym)))

(defn- params-msg [req]
  (str "params: " (pr-str (params req))))

(defn- response-msg [resp start]
  (let [status (:status resp)]
    (str "response: (" (- (System/currentTimeMillis) start) " msecs) " status
         (if (or (= status 301) (= status 302))
           (str " => " (get-in resp [:headers "Location"]))) "\n")))

(defn- error-msg [e]
  (str "error:" (pst-str e)))

(defn app
  "A core Weld app, accepting a Ring request and returning a Ring response."
  [req]
  (log :info (request-msg req))
  (let [start (System/currentTimeMillis)
        req+  (new-request req)]
    (let [method            (request-method req+)
          uri               (uri req+)
          [fn-sym r-params] (recognize method uri)
          req++             (assoc-route-params req+ r-params)
          action-fn         (resolve fn-sym)]
      (if-not action-fn
        (throwf "Routed to symbol that does not resolve: %s" fn-sym))
        (do
          (log :info (routing-msg fn-sym))
          (log :info (params-msg req++))
          (let [resp (try
                       (action-fn req++)
                       (catch Exception e
                         (log :error (error-msg e))
                         (if *handler-sym*
                           (let [handler-fn (resolve *handler-sym*)]
                             (if-not handler-fn
                               (throwf "Handler symbol does not resolve: %s", *handler-sym*)
                               (handler-fn req++)))
                           (throw e))))]
            (log :info (response-msg resp start))
            resp)))))

(ns weld.app
  (:use (weld request routing) clojure.contrib.except))

(def *logger*)

(defmacro log
  "Helper for logging around the request/response cycle."
  [msg-form]
  `(if-let [logger# *logger*]
     (if ((:test logger#) :info) ((:log logger#) ~msg-form))))

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

(defn app
  "A core Weld app, accepting a Ring request and returning a Ring response."
  [req]
  (log (request-msg req))
  (let [start (System/currentTimeMillis)
        req+  (new-request req)]
    (let [method            (request-method req+)
          uri               (uri req+)
          [fn-sym r-params] (recognize method uri)
          req++             (assoc-route-params req+ r-params)
          action-fn         (resolve fn-sym)]
      (when-not action-fn
        (throwf "Routed to symbol that does not resolve: %s" fn-sym))
      (log (routing-msg fn-sym))
      (log (params-msg req++))
      (let [resp (action-fn req++)]
        (log (response-msg resp start))
        resp))))

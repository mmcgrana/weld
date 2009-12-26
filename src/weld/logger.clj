(ns weld.logger
  (:use (clojure.contrib except def))
  (:import (java.io PrintStream)))

(defvar- level-ranks
  {:debug 0
   :info  1
   :warn  2
   :error 3
   :fatal 4})

(defvar- printers
  {:err System/err
   :out System/out})

(defn log?
  "Returns true if the logger is configured to log messages of the importance
  level. Use with log!."
  [logger message-level]
  (if-let [message-rank (level-ranks message-level)]
    (<= (:rank logger) message-rank)
    (throwf "Unrecognized level '%s': " message-level)))

(defn log!
  "Log the message to the log, regardless of importance. Use with log?."
  [logger message]
  (let [#^PrintStream printer (:printer logger)]
    (.println printer message)
    (.flush printer)))

(defmacro log
  "Log the given message if the logger is configured to log messages of the
  given importance. If logger is nil, does nothing."
  [logger-form level-form msg-form]
  `(when-let [logger# ~logger-form]
     (if (log? logger# ~level-form) (log! logger# ~msg-form))))

(defn init-logger
  "Returns a logger configured with the specified output location and
  log level.
  output - :err, :out
  level - :debug, :info, :warn, :error, :fatal"
  [output level]
  (if-let [rank (level-ranks level)]
    (if-let [printer (printers output)]
      {:rank rank :printer printer}
      (throwf "Unrecognized output '%s'" output))
    (throwf "Unrecognized level '%s'" level)))

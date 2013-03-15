(ns reload
  (:use [clojure.tools.namespace.repl :only [refresh]]))

(defn reload []
  (refresh :after 'director-musices.core/reload-ui))
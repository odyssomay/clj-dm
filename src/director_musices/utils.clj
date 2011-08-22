(ns director-musices.utils
  (:require [clojure.tools.logging :as logger]))

(defn find-i
  "Find the index of value in coll"
  [value coll]
  (let [limit (count coll)]
    (loop [i 0
           c coll]
      (if (== i limit)
        nil
        (if (= (first c) value)
          i
          (recur (inc i) (rest c)))))))

(defmacro log
  ([level msg]
   `(logger/log ~level ~msg))
  ([level e msg]
   `(logger/log ~level (str ~msg "\n    "
                            ~e   "\n        "
                            (apply str (interpose "\n        "
                                                  (take 7 (.getStackTrace ~e))))
                            "\n"))))

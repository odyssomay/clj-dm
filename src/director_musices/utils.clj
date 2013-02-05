(ns director-musices.utils
  (:use [clojure.java.io :only [file]])
  (:require [clojure.tools.logging :as logger]
            [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]]))

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

(defn new-file-dialog [& [parent]]
  (let [filename (ssw/text)
        dir (ssw/text :text (System/getProperty "user.home"))
        panel (ssw/border-panel
                :north filename :center dir
                :east (ssw/action
                        :name "browse"
                        :handler (fn [_]
                                   (ssw-chooser/choose-file
                                     parent :type "Ok" :selection-mode :dirs-only
                                     :success-fn (fn [_ f] (.setText dir (.getCanonicalPath f)))))))
        dialog (ssw/dialog :content panel :option-type :ok-cancel
                           :modal? true)]
    (if parent (.setLocationRelativeTo dialog parent))
    (.setResizable dialog false)
    (when (-> dialog ssw/pack! ssw/show!)
      (file (.getText dir) (.getText filename)))))

(defmacro with-indeterminate-progress [message & body]
  `(let [d# (ssw/frame :content (ssw/border-panel :north (ssw/label :text ~message :border 10)
                                                  :center (ssw/progress-bar :indeterminate? true :border 10)))]
     (ssw/pack! d#)
     (ssw/show! d#)
     ~@body
     (ssw/dispose! d#)))

(defn centered-component [c]
  (ssw/border-panel :center (ssw/horizontal-panel :items [:fill-h c :fill-h])))

(defmacro thread [& body]
  `(let [t# (Thread. (fn [] ~@body))]
     (.start t#)
     t#))

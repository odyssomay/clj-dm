(ns director-musices.logging
  (:require [director-musices.global :as global]
            [clojure.string :as clj-str]
            [seesaw.core :as ssw]
            [taoensso.timbre :as log]))

(def text-area (javax.swing.JTextPane.))
(def log-frame (ssw/frame :title "Director-musices log"
                          :content (ssw/scrollable text-area)
                          :size [600 :by 400]))

(defn init-cl-log-config []
  (log/set-config!
    [:prefix-fn]
    (fn [{:keys [level]}]
      (let [t (int (/ (.getUptime (java.lang.management.ManagementFactory/getRuntimeMXBean))
                      1000))
            minutes (quot t 60)
            seconds (rem t 60)]
        (str (format "%02d:%02d" minutes seconds)
             " " (-> level name clj-str/upper-case))))))

(defn init-log-frame []
  (let [kit (javax.swing.text.html.HTMLEditorKit.)
        doc (javax.swing.text.html.HTMLDocument.)
        append (fn [s] (.insertHTML kit doc (.getLength doc) s 0 0 nil))]
    (.setEditable text-area false)
    (.setEditorKit text-area kit)
    (.setDocument text-area doc)
    
    (log/set-config!
      [:appenders :log-window]
      {:min-level nil :enabled? true :async? false
       :fn (fn [{:keys [level prefix message more]}]
             (let [color (case level
                           :warn "red"
                           :fatal "red"
                           :error "red"
                           "purple")
                   bold? (#{:fatal :error} level)]
               (append (apply str
                              "<pre style='font-family: monospace; color:"
                              color
                              "; font-weight:" 
                              (if bold? "bold" "normal")
                              "'>"
                              prefix " - " message " " more ; <- note: (str) is applied to 'more'
                              ))))})))

(defn override-exception-handler [& [print-to-err?]]
  (let [old-handler (Thread/getDefaultUncaughtExceptionHandler)
        new-handler (reify Thread$UncaughtExceptionHandler
                      (uncaughtException [this thread e]
                                         ;(when print-to-err?
                                         (.uncaughtException old-handler thread e);)
                                         ;(global/configure-error :text (str "Error: " e))
                                         (log/error "BLABLA")
                                         ))]
    (Thread/setDefaultUncaughtExceptionHandler new-handler)))

(defn init []
  (init-cl-log-config)
  (init-log-frame))

(defn show-log-frame [& _]
  (.setLocationRelativeTo log-frame (global/get-frame))
  (ssw/show! log-frame))

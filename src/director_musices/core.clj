(ns director-musices.core
  (:require (director-musices [cli :as cli]
                              [global :as global]
                              [logging :as logging]
                              [menu :as menu]
                              [player :as player]
                              [util :as util])
            (director-musices.common-lisp
              [glue :as glue]
              [interpreter :as inr]
              )
            (director-musices.score
             [global :as score-global]
             [ui :as score-ui])
            (director-musices.rulepalette
              [global :as rule-global]
              [ui :as rule-ui])
            (seesaw
              [core :as ssw]
              [dnd :as ssw-dnd])
            [taoensso.timbre :as log]))

(defn bind-space [c]
  (println "binding space")
  (.put (.getInputMap
          c javax.swing.JComponent/WHEN_IN_FOCUSED_WINDOW)
        (javax.swing.KeyStroke/getKeyStroke
          "UP")
        (str ::space-pressed))
  (.put (.getActionMap c)
        (str ::space-pressed)
        (ssw/action
          :handler (fn [& _]
                     (println "space pressed!")))))

(defn input-unknown-file-type []
  (:type
    (ssw/input
      "Didn't understand that filetype"
      :choices [{:display "Midi"
                 :type :midi}
                {:display "Score"
                 :type :score}
                {:display "Rulepalette"
                 :type :rulepalette}]
      :to-string :display)))

(defn add-transfer-handler [content]
  (ssw/config!
    content
    :transfer-handler
    [:import
     [ssw-dnd/file-list-flavor
      (fn [{:keys [data]}]
        (let [f (first data)
              type (cond
                     (re-matches #".*\.midi?$" (.getName f))
                      :midi
                     (re-matches #".*\.mus$" (.getName f))
                      :score
                     (re-matches #".*\.pal$" (.getName f))
                      :rulepalette
                     :else (input-unknown-file-type))]
          (case type
            :midi (score-ui/load-score-from-midi-file f)
            :score (score-ui/load-score-from-file f)
            :rulepalette (rule-ui/load-rulepalette-from-file f)
            nil)))
      ]]))

(defn director-musices [& args]
  (global/native!)
  (logging/init)
  (cli/parse-args args)
  (global/init)
  (rule-ui/init)
  (score-ui/init)
  (let [arg? (fn [arg] (some #(= % arg) args))
        fr (global/get-frame)]
    (ssw/config! fr
                 :title "Director Musices"
                 :menubar (menu/menubar)
                 :size [800 :by 600]
                 :on-close (if (global/get-arg :exit) :exit :dispose)
                 )
    (ssw/config! (global/get-main-panel)
                 :north (menu/toolbar-panel)
                 :center (ssw/top-bottom-split
                           (score-global/get-score-panel)
                           (rule-global/get-rulepalette-panel)
                           :divider-location 0.5
                           :resize-weight 0.5
                           :one-touch-expandable? true))
    (add-transfer-handler (global/get-main-panel))
    ;(bind-space (global/get-main-panel))
    (when (global/get-arg :cl-repl) (inr/repl))
    (log/info "Using tmp directory" (util/tmp-dir))
    (.setLocationRelativeTo fr nil)
    (ssw/show! fr)
    (util/thread (glue/init-dm))))

(defn reload-ui [] (director-musices "--no-exit"))

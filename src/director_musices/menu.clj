(ns director-musices.menu
  (:use [clojure.java.io :only [resource]])
  (:require (director-musices
              [global :as global]
              [player :as player]
              [util :as util])
            [director-musices.logging :as logging]
            (director-musices.common-lisp
              [glue :as glue]
              [command-line :as command-line])
            (director-musices.score
              [global :as score-global]
              [ui :as score-ui])
            [director-musices.rulepalette.ui :as rule-ui]
            (seesaw
              [core :as ssw]
              [mig :as ssw-mig])))

(defn reload-score []
  (score-ui/reload-score))

(defn file-menu []
  (ssw/menu
    :text "File"
    :items
    [(ssw/action :name "Open Score..."
                 :handler score-ui/choose-and-open-score)
     (ssw/action :name "Open Test Score"
                 :handler score-ui/open-test-score)
     (ssw/action :name "Open Performance..."
                 :handler score-ui/choose-and-open-performance)
     (score-global/a-if-score :name "Save Score As..."
                              :handler score-ui/choose-and-save-score)
     (score-global/a-if-score :name "Save Performance As..."
                              :handler score-ui/choose-and-save-performance)
     :separator
     (score-global/a-if-score :name "Save pdm As..."
                              :handler score-ui/choose-and-save-pdm)
     :separator
     (ssw/action :name "Import Score from Midifile..."
                 :handler score-ui/choose-and-open-midi)
     (score-global/a-if-score :name "Export Performance to Midifile..."
                              :handler score-ui/choose-and-save-midi)
     :separator
     (ssw/action :name "Open Rulepalette..."
                 :handler rule-ui/choose-and-open-rulepalette)
     (ssw/action :name "Open Default Rulepalette"
                 :handler rule-ui/open-default-rulepalette)
     :separator
     (ssw/action :name "Quit"
                 :handler (fn [&_ ] (System/exit 0)))]))

(defmacro run-action [text & body]
  `(util/thread
     (global/show-info-panel :loading ~text)
     ~@body
     (reload-score)
     (global/hide-info-panel)))

(def edit-menu
  (ssw/menu
    :text "Edit"
    :items
    [(ssw/action :name "Tempo"
                 :handler (fn [& _] (when-let [bpm-raw (ssw/input "Set new tempo"  :title "Set tempo" 
                                                                  :value (.javaInstance (glue/eval-dm "(get-first 'mm)")))]
                                      (let [bpm (read-string bpm-raw)]
                                        (run-action "Setting tempo"
                                          (glue/eval-dm (str "(set-tempo " bpm ")")))))))
     (ssw/action :name "Octave"
                 :handler (fn [& _] (when-let [raw (ssw/input "Transpose octave" :title "Set octave" :value 1)]
                                      (let [o (read-string raw)]
                                        (run-action "Transposing octave"
                                          (glue/eval-dm (str "(trans-octave " o ")")))))))
     (ssw/action :name "Meter"
                 :handler (fn [& _] 
                            (let [m1 (.javaInstance (glue/eval-dm "(first (get-first 'meter))"))
                                  m2 (.javaInstance (glue/eval-dm "(second (get-first 'meter))"))]
                              (when-let [raw (ssw/input "Set meter" :title "Set meter" 
                                                        :value (str m1 "/" m2))]
                                (run-action "Setting meter"
                                  (let [[nm1 nm2] (map read-string (.split raw "/"))]
                                    (glue/eval-dm (str "(set-meter " nm1 " " nm2 ")"))))))))
     (ssw/action :name "Key"
                 :handler (fn [& _] 
                            (when-let [k (ssw/input "Set key" :title "Set key"
                                                    :value (.javaInstance (glue/eval-dm "(get-first 'key)")))]
                              (when-let [m (ssw/input "Set modus" :title "Set modus"
                                                      :value (.javaInstance (glue/eval-dm "(get-first 'modus)")))]
                                (run-action "Setting key"
                                  (glue/eval-dm (str "(set-first 'key \"" k "\")"))
                                  (glue/eval-dm (str "(set-first 'modus \"" m "\")")))))))
     :separator
     (ssw/action :name "Remove Parameter"
                 :handler (fn [& _] (when-let [raw (ssw/input "Remove parameter" :title "Remove parameter")]
                                      (let [p (read-string raw)]
                                        (run-action "Removing parameter"
                                          (glue/eval-dm (str "(rem-all '" p ")")))))))
     (ssw/action :name "Remove all phrase marks"
                 :handler (fn [& _]
                            (let [d (doto (ssw/dialog :content "Are you sure?"
                                                      :title "Remove all phrase marks confirmation"
                                                      :option-type :ok-cancel)
                                      (.setLocationRelativeTo (global/get-frame))
                                      ssw/pack!)]
                              (when-let [_ (ssw/show! d)]
                                (run-action "Removing all phrase marks"
                                            (glue/eval-dm (str "(rem-all 'phrase-start) (rem-all 'phrase-end)")))))))
     :separator
     (ssw/action :name "Reset Soundlevel"
                 :handler (fn [& _]
                            (run-action "Resetting sound level"
                              (glue/eval-dm "(reset-sound-level)"))))
     (ssw/action :name "Rebar"
                 :handler (fn [& _] (run-action "Rebaring"
                                      (glue/eval-dm "(rebar)"))))
     (ssw/action :name "Convert chord list to chord name"
                 :handler (fn [& _] (run-action "Converting chord list to chord name"
                                      (glue/eval-dm "(convert-chord-list-to-chord-name)"))))
     (ssw/action :name "Distribute phrase analysis"
                 :handler (fn [& _] (run-action "Distributing phrase analysis"
                                      (glue/eval-dm "(distribute-phrase-analysis)"))))
     :separator
     (ssw/action :name "Print all score vars"
                 :handler (fn [& _] (glue/eval-dm "(print-music)")))
     (ssw/action :name "Print all score vars round"
                 :handler (fn [& _] (glue/eval-dm "(print-music-round)")))
     :separator
     (ssw/action :name "Transpose from major to minor"
                 :handler (fn [_] (run-action "Transposing from major to minor"
                                    (glue/eval-dm "(transpose-from-major-to-minor)"))))
     (ssw/action :name "Transpose from minor to major"
                 :handler (fn [_] (run-action "Transposing from minor to major"
                                    (glue/eval-dm "(transpose-from-minor-to-major)"))))
     ]))

(def help-menu
  (ssw/menu
    :text "Help"
    :items
    [(ssw/action :name "Log" :handler logging/show-log-frame)
     (ssw/action :name "Command Line"
                 :handler command-line/show!)
     :separator
     (ssw/action :name "Director-musices website"
                 :handler
                 (fn [_] (util/open-website
                           "https://github.com/odyssomay/clj-dm#readme")))]))

(defn menubar []
  (ssw/menubar :items 
               [(file-menu)
                edit-menu
                help-menu]))

(defn start-pause-action []
  (let [a (score-global/a-if-score
            :icon (resource "icons/play.png") :tip "play"
            :handler (fn [_]
                       (util/thread
                         (score-ui/reload-player-if-changed!)
                         (player/start!))))
        play! (fn []
                (util/thread
                  (score-ui/reload-player-if-changed!)
                  (player/start!))
                (ssw/config! a
                             :icon (resource "icons/pause.png")
                             :tip "pause"))
        pause! (fn []
                 (player/pause!)
                 (ssw/config! a
                              :icon (resource "icons/play.png")
                              :tip "play"))
        stop-action
        (score-global/a-if-score
          :icon (resource "icons/stop.png")
          :handler (fn [_]
                     (pause!)
                     (player/stop!))
          :tip "stop")
        config-action
        (ssw/action :icon (resource "icons/gear.png")
                    :handler (fn [_]
                               (pause!)
                               (player/choose-midi-device))
                    :tip "Select midi device")]
    (ssw/config!
      a :handler (fn [_]
                   (when-let [s (player/get-sequencer)]
                     (if (.isRunning s)
                       (pause!) (play!)))))
    {:stop stop-action
     :play a
     :config config-action}))

(defn toolbar* []
  (let [{:keys [stop play config]} (start-pause-action)]
    (ssw/toolbar
      :floatable? false
      :items
      [play :separator stop
       :separator config
       :separator
       "scale"
       (ssw/slider :value 100
                   :min 20
                   :max 180
                   :major-tick-spacing 40
                   :minor-tick-spacing 10
                   :snap-to-ticks? true
                   :paint-ticks? true
                   :size [200 :by 30]
                   :listen [:change
                            (fn [e]
                              (let [s (.getSource e)
                                    value (double (/ (.getValue (.getSource e))
                                                     100))]
                                (if (.getValueIsAdjusting s)
                                  (score-global/temporary-scale! value)
                                  (score-global/scale! value))))])])))

(defn toolbar []
  (ssw-mig/mig-panel
    :constraints ["insets 0, fill"]
    :items
    [[(toolbar*)]
     [(global/get-info-panel) "dock east"]]))

(defn toolbar-panel [] (toolbar))
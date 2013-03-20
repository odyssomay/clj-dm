(ns director-musices.menu
  (:use [clojure.java.io :only [resource]])
  (:require (director-musices
              [global :as global]
              [player :as player]
              [util :as util])
            [director-musices.logging :as logging]
            (director-musices.common-lisp
              [glue :as glue])
            (director-musices.score
              [global :as score-global]
              [menu :as score-menu]
              [ui :as score-ui])
            [seesaw.core :as ssw]
            ))

(defn reload-score []
  (score-ui/reload-score))

(def edit-menu
  (ssw/menu
    :text "Edit"
    :items
    [(ssw/action :name "Tempo"
                 :handler (fn [& _] (when-let [bpm-raw (ssw/input "Set new tempo"  :title "Set tempo" 
                                                                  :value (.javaInstance (glue/eval-dm "(get-first 'mm)")))]
                                      (let [bpm (read-string bpm-raw)]
                                        (glue/eval-dm (str "(set-tempo " bpm ")"))
                                        (reload-score)))))
     (ssw/action :name "Octave"
                 :handler (fn [& _] (when-let [raw (ssw/input "Transpose octave" :title "Set octave" :value 1)]
                                      (let [o (read-string raw)]
                                        (glue/eval-dm (str "(trans-octave " o ")"))
                                        (reload-score)))))
     (ssw/action :name "Meter"
                 :handler (fn [& _] 
                            (let [m1 (.javaInstance (glue/eval-dm "(first (get-first 'meter))"))
                                  m2 (.javaInstance (glue/eval-dm "(second (get-first 'meter))"))]
                              (when-let [raw (ssw/input "Set meter" :title "Set meter" 
                                                        :value (str m1 "/" m2))]
                                (let [[nm1 nm2] (map read-string (.split raw "/"))]
                                  (glue/eval-dm (str "(set-meter " nm1 " " nm2 ")")))))
                            (reload-score)))
     (ssw/action :name "Key"
                 :handler (fn [& _] 
                            (when-let [k (ssw/input "Set key" :title "Set key"
                                                    :value (.javaInstance (glue/eval-dm "(get-first 'key)")))]
                              (when-let [m (ssw/input "Set modus" :title "Set modus"
                                                      :value (.javaInstance (glue/eval-dm "(get-first 'modus)")))]
                                (glue/eval-dm (str "(set-first 'key \"" k "\")"))
                                (glue/eval-dm (str "(set-first 'modus \"" m "\")"))))
                            (reload-score)))
     :separator
     (ssw/action :name "Remove Parameter"
                 :handler (fn [& _] (when-let [raw (ssw/input "Remove parameter" :title "Remove parameter")]
                                      (let [p (read-string raw)]
                                        (glue/eval-dm (str "(rem-all '" p ")"))))
                            (reload-score)))
     (ssw/action :name "Reset Soundlevel"
                 :handler (fn [& _] (glue/eval-dm "(reset-sound-level)")
                            (reload-score)))
     (ssw/action :name "Rebar"
                 :handler (fn [& _] (glue/eval-dm "(rebar)")
                            (reload-score)))
     (ssw/action :name "Convert chord list to chord name"
                 :handler (fn [& _] (glue/eval-dm "(convert-chord-list-to-chord-name)")
                            (reload-score)))
     (ssw/action :name "Distribute phrase analysis"
                 :handler (fn [& _] (glue/eval-dm "(distribute-phrase-analysis)")
                            (reload-score)))
     :separator
     (ssw/action :name "Print all score vars"
                 :handler (fn [& _] (glue/eval-dm "(print-music)")
                            (reload-score)))
     (ssw/action :name "Print all score vars round"
                 :handler (fn [& _] (glue/eval-dm "(print-music-round)")
                            (reload-score)))
     :separator
     (ssw/action :name "Transpose from major to minor"
                 :handler (fn [_] (glue/eval-dm "(transpose-from-major-to-minor)")
                            (reload-score)))
     (ssw/action :name "Transpose from minor to major"
                 :handler (fn [_] (glue/eval-dm "(transpose-from-minor-to-major)")
                            (reload-score)))
     ]))

(def help-menu
  (ssw/menu
    :text "Help"
    :items
    [(ssw/action :name "Log" :handler logging/show-log-frame)
     :separator
     (ssw/action :name "Director-musices website"
                 :handler
                 (fn [_] (util/open-website
                           "https://github.com/odyssomay/clj-dm#readme")))
     ]))

(defn menubar []
  (ssw/menubar :items 
               [score-menu/file-menu
                edit-menu
                help-menu
                ]))

(defn toolbar []
  (ssw/toolbar
    :floatable? true
    :items
    [(ssw/action :icon (resource "icons/play.png") :tip "play"
                 :handler (fn [_] (player/start!)))
     (ssw/action :icon (resource "icons/pause.png") :tip "pause"
                 :handler (fn [_] (player/pause!)))
     :separator
     (ssw/action :icon (resource "icons/stop.png")
                 :handler (fn [_] (player/stop!))
                 :tip "stop")
     :separator
     (ssw/action :icon (resource "icons/gear.png")
                 :handler (fn [_] (player/choose-midi-device))
                 :tip "Select midi device")
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
                 :listen [:change (fn [e]
                                    (score-global/scale!
                                      (double (/ (.getValue (.getSource e))
                                                 100)))
                                    )])
     ]))

(defn toolbar-panel []
  (ssw/border-panel :center (toolbar)
                    :south player/indicator-panel))
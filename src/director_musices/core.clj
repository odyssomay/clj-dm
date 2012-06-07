(ns director-musices.core
  (:gen-class)
  (:use (director-musices rulepalette player score)
;        (director-musices.gui score)
        )
  (:require [seesaw.core :as ssw]
            (director-musices [glue :as glue]
                              [interpreter :as inr]
                              [score :as score])))

(def file-menu
  (ssw/menu
    :text "File"
    :items 
    [(ssw/action :name "Open Score"
                 :handler choose-and-open-score)
     (ssw/action :name "Save Score"
                 :handler choose-and-save-score)
     (ssw/action :name "Save Performance"
                 :handler choose-and-save-performance)
     (ssw/separator)
     (ssw/action :name "Import Midifile"
                 :handler choose-and-open-midi)
     (ssw/action :name "Save Midifile"
                 :handler choose-and-save-midi)
     (ssw/separator)
     (ssw/action :name "Quit"
                 :handler (fn [&_ ] (System/exit 0)))
     ]))

(def edit-menu
  (ssw/menu
    :text "Edit"
    :items
    [(ssw/action :name "Tempo"
                 :handler (fn [& _] (when-let [bpm-raw (ssw/input "Set new tempo"  :title "Set tempo" 
                                                                  :value (.javaInstance (inr/eval-abcl-dm "(get-first 'mm)")))]
                                      (let [bpm (read-string bpm-raw)]
                                             (inr/eval-abcl-dm (str "(set-tempo " bpm ")"))))))
     (ssw/action :name "Octave"
                 :handler (fn [& _] (when-let [raw (ssw/input "Transpose octave" :title "Set octave" :value 1)]
                                      (let [o (read-string raw)]
                                        (inr/eval-abcl-dm (str "(trans-octave " o ")"))))))
     (ssw/action :name "Meter"
                 :handler (fn [& _] 
                            (let [m1 (.javaInstance (inr/eval-abcl-dm "(first (get-first 'meter))"))
                                  m2 (.javaInstance (inr/eval-abcl-dm "(second (get-first 'meter))"))]
                              (when-let [raw (ssw/input "Set meter" :title "Set meter" 
                                                        :value (str m1 "/" m2))]
                                (let [[nm1 nm2] (map read-string (.split raw "/"))]
                                  (inr/eval-abcl-dm (str "(set-meter " nm1 " " nm2 ")")))))))
     (ssw/action :name "Key"
                 :handler (fn [& _] 
                            (when-let [k (ssw/input "Set key" :title "Set key"
                                                    :value (.javaInstance (inr/eval-abcl-dm "(get-first 'key)")))]
                              (when-let [m (ssw/input "Set modus" :title "Set modus"
                                                      :value (.javaInstance (inr/eval-abcl-dm "(get-first 'modus)")))]
                                (inr/eval-abcl-dm (str "(set-first 'key \"" k "\")"))
                                (inr/eval-abcl-dm (str "(set-first 'modus \"" m "\")"))))))
     (ssw/action :name "Remove Parameter"
                 :handler (fn [& _] (when-let [raw (ssw/input "Remove parameter" :title "Remove parameter")]
                                      (let [p (read-string raw)]
                                        (inr/eval-abcl-dm (str "(rem-all '" p ")"))))))
     (ssw/action :name "Reset Soundlevel"
                 :handler (fn [& _] (inr/eval-abcl-dm "(reset-sound-level)") (score/reload-score-panel)))
     (ssw/action :name "Rebar"
                 :handler (fn [& _] (inr/eval-abcl-dm "(rebar)") (score/reload-score-panel)))
     ]))

(def rules-menu
  (ssw/menu
    :text "Rules"
    :items
    [(ssw/action :name "Open Rulepalette"
                 :handler choose-and-open-rulepalette)
     (ssw/action :name "Open Default Rulepalette" :handler open-default-rulepalette)
     (ssw/action :name "Save Rulepalette")
     (ssw/separator)
     (ssw/action :name "Apply current Rulepalette"
                 :handler apply-current-rulepalette)
     (ssw/action :name "Apply all Rulepalettes"
                 :handler apply-all-rulepalettes)
     (let [cb (ssw/checkbox :text "Reset on Apply")]
       (ssw/listen cb :selection (fn [& _] (set-reset-on-apply (.isSelected cb))))
       cb)
     ]))

(def help-menu
  (ssw/menu
    :text "Help"
    :items 
    [(ssw/action :name "About")]))

(defn init-menu-bar []
  (ssw/menubar :items [file-menu
                       edit-menu
                       rules-menu
                       help-menu
                       ]))

(defn director-musices [& args]
  (let [fr (ssw/frame 
             :title "Director Musices"
             :content (ssw/border-panel :north player-panel :center (ssw/top-bottom-split score-panel
                                                                                          rulepalette-container))
             :menubar (init-menu-bar)
             :size [800 :by 600]
             :on-close :exit
             )]
    (when (some #(= % "-cl-repl") args) (inr/repl)) 
    (ssw/show! fr)
    nil))


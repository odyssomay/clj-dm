(ns director-musices.menu
  (:require [director-musices.logging :as logging]
            (director-musices.common-lisp
              [glue :as glue])
            [director-musices.rulepalette.menu :as rulepalette-menu]
            (director-musices.score
              [menu :as score-menu]
              [ui :as score-ui])
            [seesaw.core :as ssw]
            ))

(def edit-menu
  (ssw/menu
    :text "Edit"
    :items
    [(ssw/action :name "Tempo"
                 :handler (fn [& _] (when-let [bpm-raw (ssw/input "Set new tempo"  :title "Set tempo" 
                                                                  :value (.javaInstance (glue/eval-dm "(get-first 'mm)")))]
                                      (let [bpm (read-string bpm-raw)]
                                             (glue/eval-dm (str "(set-tempo " bpm ")"))))))
     (ssw/action :name "Octave"
                 :handler (fn [& _] (when-let [raw (ssw/input "Transpose octave" :title "Set octave" :value 1)]
                                      (let [o (read-string raw)]
                                        (glue/eval-dm (str "(trans-octave " o ")"))))))
     (ssw/action :name "Meter"
                 :handler (fn [& _] 
                            (let [m1 (.javaInstance (glue/eval-dm "(first (get-first 'meter))"))
                                  m2 (.javaInstance (glue/eval-dm "(second (get-first 'meter))"))]
                              (when-let [raw (ssw/input "Set meter" :title "Set meter" 
                                                        :value (str m1 "/" m2))]
                                (let [[nm1 nm2] (map read-string (.split raw "/"))]
                                  (glue/eval-dm (str "(set-meter " nm1 " " nm2 ")")))))))
     (ssw/action :name "Key"
                 :handler (fn [& _] 
                            (when-let [k (ssw/input "Set key" :title "Set key"
                                                    :value (.javaInstance (glue/eval-dm "(get-first 'key)")))]
                              (when-let [m (ssw/input "Set modus" :title "Set modus"
                                                      :value (.javaInstance (glue/eval-dm "(get-first 'modus)")))]
                                (glue/eval-dm (str "(set-first 'key \"" k "\")"))
                                (glue/eval-dm (str "(set-first 'modus \"" m "\")"))))))
     (ssw/action :name "Remove Parameter"
                 :handler (fn [& _] (when-let [raw (ssw/input "Remove parameter" :title "Remove parameter")]
                                      (let [p (read-string raw)]
                                        (glue/eval-dm (str "(rem-all '" p ")"))))))
     (ssw/action :name "Reset Soundlevel"
                 :handler (fn [& _] (glue/eval-dm "(reset-sound-level)") (score-menu/reload-score-panel)))
     (ssw/action :name "Rebar"
                 :handler (fn [& _] (glue/eval-dm "(rebar)") (score-menu/reload-score-panel)))
     ]))

(def help-menu
  (ssw/menu
    :text "Help"
    :items
    [(ssw/action :name "Log" :handler logging/show-log-frame)
     :separator
     (ssw/action :name "About")]))

(defn menubar []
  (ssw/menubar :items 
               [score-menu/file-menu
                edit-menu
                rulepalette-menu/rules-menu
                help-menu
                ]))
(ns director-musices.menu
  (:require (director-musices.common-lisp
              [glue :as glue]
              [interpreter :as inr]
              )
            [director-musices.rulepalette.menu :as rulepalette-menu]
            (director-musices.score
              [score :as score]
              [menu :as score-menu])
            [seesaw.core :as ssw]
            ))

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

(def help-menu
  (ssw/menu
    :text "Help"
    :items
    [(ssw/action :name "About")]))

(defn menubar []
  (ssw/menubar :items 
               [score-menu/file-menu
                edit-menu
                rulepalette-menu/rules-menu
                help-menu
                ]))
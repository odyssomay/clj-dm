(ns director-musices.core
  (:use director-musices.rulepalette.rulepalette
        director-musices.player
        director-musices.score.score
        )
  (:require [seesaw.core :as ssw]
            (director-musices [global :as global]
                              [menu :as menu]
                              [utils :as util])
            (director-musices.common-lisp
              [glue :as glue]
              [interpreter :as inr]
              )
            [director-musices.score.score :as score]
            ))

(defn director-musices [& args]
  (let [arg? (fn [arg] (some #(= % arg) args))
        fr (global/get-frame)]
    (ssw/config! fr
                 :title "Director Musices"
                 :menubar (menu/menubar)
                 :size [800 :by 600]
                 :on-close (if (arg? "-no-exit") :hide :exit)
                 )
    (ssw/config! (global/get-main-panel)
                 :north player-panel 
                 :center (ssw/top-bottom-split 
                           score-panel
                           rulepalette-panel
                           :divider-location 0.5))
    (when (some #(= % "-cl-repl") args) (inr/repl))
    (ssw/show! fr)
    (util/thread (glue/init-dm))
    nil))

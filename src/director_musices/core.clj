(ns director-musices.core
  (:require [seesaw.core :as ssw]
            (director-musices [global :as global]
                              [menu :as menu]
                              [player :as player]
                              [util :as util])
            (director-musices.common-lisp
              [glue :as glue]
              [interpreter :as inr]
              )
            [director-musices.score.global :as score-global]
            [director-musices.rulepalette.global :as rule-global]
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
                 :north player/player-panel
                 :center (ssw/top-bottom-split 
                           score-global/score-panel
                           rule-global/rulepalette-panel
                           :divider-location 0.5))
    (when (some #(= % "-cl-repl") args) (inr/repl))
    (ssw/show! fr)
    (util/thread (glue/init-dm))
    nil))

(defn reload-fn [] (director-musices "-no-exit"))
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
            (director-musices.score
             [global :as score-global]
             [ui :as score-ui])
            (director-musices.rulepalette
              [global :as rule-global]
              [ui :as rule-ui])
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
    (let [t (util/thread (glue/init-dm))]
      (if (arg? "-return-thread")
        t
        nil))))

(defn reload-ui []
  (.join (director-musices "-no-exit" "-return-thread"))
  (score-ui/reload-ui)
  (rule-ui/reload-ui)
  )
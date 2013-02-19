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
            [seesaw.core :as ssw]
            [taoensso.timbre :as log]
            ))

(defn director-musices [& args]
  (global/native!)
  (global/init)
  (logging/init)
  (cli/parse-args args)
  (let [arg? (fn [arg] (some #(= % arg) args))
        fr (global/get-frame)]
    (ssw/config! fr
                 :title "Director Musices"
                 :menubar (menu/menubar)
                 :size [800 :by 600]
                 :on-close (if (global/get-arg :exit) :exit :hide)
                 )
    (ssw/config! (global/get-main-panel)
                 :center (ssw/top-bottom-split
                           score-global/score-panel
                           rule-global/rulepalette-panel
                           :divider-location 0.5))
    (when (global/get-arg :cl-repl) (inr/repl))
    (log/info "Using tmp directory" (util/tmp-dir))
    (rule-ui/init)
    (score-ui/init)
    (ssw/show! fr)
    (let [t (util/thread (glue/init-dm))]
      (if (global/get-arg :return-thread)
        t
        nil))))

(defn reload-ui []
  (.join (director-musices "--no-exit" "--return-thread"))
  ;(score-ui/reload-ui)
  ;(rule-ui/reload-ui)
  )

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
                 :on-close (if (global/get-arg :exit) :exit :hide)
                 )
    (ssw/config! (global/get-main-panel)
                 :center (ssw/top-bottom-split
                           (score-global/get-score-panel)
                           (rule-global/get-rulepalette-panel)
                           :divider-location 0.5))
    (when (global/get-arg :cl-repl) (inr/repl))
    (log/info "Using tmp directory" (util/tmp-dir))
    (.setLocationRelativeTo fr nil)
    (ssw/show! fr)
    (util/thread (glue/init-dm))
    ))

(defn reload-ui [] (director-musices "--no-exit"))

(ns director-musices.core
  (:require (director-musices [global :as global]
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
            clojure.tools.cli
            ))

(defn director-musices [& args]
  (let [help? (some #{"--help" "-help" "-h"} args)
        [arg-map invalid help-str]
        (clojure.tools.cli/cli
          (if help? [] args)
          ["--dev" "Starts a developer version." :default false :flag true]
          ["--exit" "Exit after window has closed." :default true :flag true]
          ["--return-thread" "Returns a startup thread." :default false :flag true]
          ["--cl-repl" "Starts a common-lisp repl. Note that the repl will be started on the command line." :default false :flag true]
          )]
    (global/set-arg-map arg-map)
    (when help?
      (println help-str)
      (System/exit 0))
    (when (> (count invalid) 0)
      (log/warn "these arguments are invalid:" invalid))
    )
  (let [arg? (fn [arg] (some #(= % arg) args))
        fr (global/get-frame)]
    (ssw/config! fr
                 :title "Director Musices"
                 :menubar (menu/menubar)
                 :size [800 :by 600]
                 :on-close (if (global/get-arg :exit) :exit :hide)
                 )
    (ssw/config! (global/get-main-panel)
                 :north player/player-panel
                 :center (ssw/top-bottom-split
                           score-global/score-panel
                           rule-global/rulepalette-panel
                           :divider-location 0.5))
    (when (global/get-arg :cl-repl) (inr/repl))
    (ssw/show! fr)
    (let [t (util/thread (glue/init-dm))]
      (if (global/get-arg :return-thread)
        t
        nil))))

(defn reload-ui []
  (.join (director-musices "--no-exit" "--return-thread"))
  (score-ui/reload-ui)
  (rule-ui/reload-ui))
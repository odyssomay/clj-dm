(ns director-musices.cli
  (:require [director-musices.global :as global]
            clojure.tools.cli
            [taoensso.timbre :as log]))

(defn parse-args [args]
  (let [help? (some #{"--help" "-help" "-h"} args)
        [arg-map invalid help-str]
        (clojure.tools.cli/cli
          (if help? [] args)
          ["--dev" "Starts a developer version." :default false :flag true]
          ["--dm-path" "Define a path to director-musices source files." :default nil]
          ["--watch" "Watch common-lisp files for changes and automatically reload them on change."
           :default false :flag true]
          ["--exit" "Exit after window has closed." :default true :flag true]
          ["--cl-repl" "Starts a common-lisp repl. Note that the repl will be started on the command line." 
           :default false :flag true]
          )]
    (if (and (:watch arg-map)
             (not (:dm-path arg-map)))
      (log/warn "--watch cannot be used without --dm-path"))
    (global/set-arg-map arg-map)
    (when help?
      (println help-str)
      (System/exit 0))
    (when (> (count invalid) 0)
      (log/warn "these arguments are invalid:" invalid))
    )
  )
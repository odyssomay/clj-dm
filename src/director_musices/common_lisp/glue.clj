(ns director-musices.common-lisp.glue
  (:use clojure.java.io
        director-musices.common-lisp.interpreter
        (director-musices 
          [util :only [with-indeterminate-progress]]))
  (:require (director-musices [global :as global])
            [seesaw.core :as ssw]))

(let [dm-init? (atom nil)]
  (defn init-dm []
    (when-not @dm-init?
      (println "initing dm")
      (global/update-progress-bar :large-text "Loading files"
                                  :indeterminate? false
                                  :percent-done 0)
      (global/show-progress-bar)
      (load-multiple-abcl-with-progress
        {:percent-done #(global/update-progress-bar :percent-done %)
         :current-file #(global/update-progress-bar :small-text %)}
        "dm:"
        ["package-dm.lsp"]
        "dm:lib-core:"
        ["scoreobjects.lsp" "basicmacros.lsp" "infixmath.lsp" "musicio.lsp" 
         "rulemacros.lsp" "parallelrulemacros.lsp" 
         "dm-objects.lsp" "initconvert.lsp" "save-pdm-score.lsp" 
         "rule-groups.lsp" "syntobjects.lsp" "shapeobjects.lsp"
         "midifileoutput.lsp" "midifileinput.lsp" "playlist.lsp" "midibasic-lw.lsp"]
        "dm:"
        ["init.lsp"]
        "dm:rules:"
        ["frules1.lsp" "frules2.lsp" "Intonation.lsp"
         "FinalRitard.lsp" "utilityrules.lsp" "Punctuation.lsp"
         "phrasearch.lsp" "SyncOnMel.lsp"])
        (global/hide-progress-bar)
        (reset! dm-init? true)
        )))

(defn eval-dm [s]
  (init-dm)
  (eval-abcl "(in-package :dm)")
  (eval-abcl s))

(defn save-midi-to-path [path]
  (eval-dm (str "(save-performance-midifile1-fpath \"" (abcl-path path) "\")")))
(ns director-musices.common-lisp.glue
  (:use [clojure.java.io :only [resource]]
        director-musices.common-lisp.interpreter
        (director-musices 
          [util :only [with-indeterminate-progress]]))
  (:require (director-musices [global :as global])
            clojure.string
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
        "dm"
        ["package-dm"]
        "dm:lib-core"
        ["scoreobjects" "basicmacros" "infixmath" "musicio" 
         "rulemacros" "parallelrulemacros" 
         "dm-objects" "initconvert" "save-pdm-score" 
         "rule-groups" "syntobjects" "shapeobjects"
         "midifileoutput" "midifileinput" "playlist" "midibasic-lw"]
        "dm"
        ["init"]
        "dm:rules"
        ["frules1" "frules2" "Intonation"
         "FinalRitard" "utilityrules" "Punctuation"
         "phrasearch" "SyncOnMel"])
        (global/hide-progress-bar)
        (reset! dm-init? true)
        )))

; (defn read-dm-paths-file [path]
;   (let [lines (clojure.string/split-lines (slurp path))]
;     (map #(vec [(ffirst %) (second %)])
;          (partition 2 (partition-by #(.startsWith % " ")
;                                ;#(re-matches #"\s+.+" %) 
;                                lines)))
;     ;lines
;     ))

; (prn (read-dm-paths-file (resource "dm/paths")))
    
(defn eval-dm [s]
  (init-dm)
  (eval-abcl "(in-package :dm)")
  (eval-abcl s))

(defn save-midi-to-path [path]
  (eval-dm (str "(save-performance-midifile1-fpath \"" (abcl-path path) "\")")))

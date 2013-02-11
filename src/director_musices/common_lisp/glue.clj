(ns director-musices.common-lisp.glue
  (:use [clojure.java.io :only [resource]]
        director-musices.common-lisp.interpreter
        (director-musices 
          [util :only [with-indeterminate-progress]]))
  (:require (director-musices [global :as global])
            clojure.string
            [taoensso.timbre :as log]
            [seesaw.core :as ssw]))

(defn parse-dm-paths-file [s]
  (let [lines (clojure.string/split-lines s)]
    (->> lines
         (partition-by #(.startsWith % " ")
                       ;#(re-matches #"\s+.+" %)
                       )
         (partition 2)
         (map #(vec [(ffirst %) (map clojure.string/trim (second %))]))
         (reduce concat))))

(defn read-dm-paths-file [path]
  (log/info "loading dm from" path)
  (parse-dm-paths-file (slurp path)))

(let [dm-init? (atom nil)]
  (defn init-dm []
    (when-not @dm-init?
      (log/info "initing dm")
      (global/update-progress-bar :large-text "Loading files"
                                  :indeterminate? false
                                  :percent-done 0)
      (global/show-progress-bar)
      (let [d (resource "dm/paths")
            dm-paths (read-dm-paths-file
                       (if-let [p (global/get-arg :dm-path)]
                         (let [f (java.io.File. p "paths")]
                           (if (.exists f)
                             f
                             (do (log/warn "'paths' file was not found in" p 
                                           ", using in-built.")
                               d)))
                         d))]
        (apply load-multiple-abcl-with-progress
               {:percent-done #(global/update-progress-bar :percent-done %)
                :current-file #(global/update-progress-bar :small-text %)}
               dm-paths)
        (global/hide-progress-bar)
        (reset! dm-init? true)
        ))))
    
(defn eval-dm [s]
  (init-dm)
  (eval-abcl "(in-package :dm)")
  (eval-abcl s))

(defn save-midi-to-path [path]
  (eval-dm (str "(save-performance-midifile1-fpath \"" (abcl-path path) "\")")))

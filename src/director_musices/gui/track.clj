(ns director-musices.gui.track
  (:use clojure.tools.logging
        (director-musices.gui note)))

(defn init-track-pvariables [{nos :notes :as track}]
  (assoc track :notes
         (vec (map init-note-pvariables nos))))

(defn map->strlist [m]
  (apply str
         (map (fn [[k v]]
                (str " " k " " 
                     (if (string? v) (str "\"" v "\"") v) 
                     "\n")) m)))

(defn track->string [track]
  (str "mono-track\n" 
       (map->strlist (:options track))
       (apply str (map note->string (:notes track))))) 

;; abc4j

(defn add-legato [track]
  (let [start (filter :legato-start (:notes track))
        end (filter :legato-end (:notes track))
        notes (interleave start end)]
    (if-not (even? (count notes))
      (error "legato-start and legato-end doesn't match")
      (dorun (map (fn [[note1 note2]]
                    (let [abc4j_note1 (:note (:abc4j note1))
                          abc4j_note2 (:note (:abc4j note2))
                          slur (abc.notation.SlurDefinition.)]
                      (.setStart slur abc4j_note1)
                      (.setEnd slur abc4j_note2)
                      (.setSlurDefinition abc4j_note1 slur)
                      (.setSlurDefinition abc4j_note2 slur))) 
                  (partition 2 notes)))))
  track)

(defn track-to-abc4j [track]
  (assoc track :notes 
         (map note-to-abc4j (:notes track))))

(defn add-track-to-score [track score]
  (dorun (->> (track-to-abc4j track)
              add-legato
              :notes
              (map #(add-note-to-score % score)))))


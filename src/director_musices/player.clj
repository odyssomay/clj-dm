(ns director-musices.player
  (:use (director-musices [glue :only [save-midi-to-path]])
        [clojure.java.io :only [file resource]])
  (:require [seesaw.core :as ssw])
  (:import (javax.sound.midi MidiSystem)))

;(def dynamic-sequence
;  (atom (MidiSystem/getSequence (file "/home/odyssomay/Dropbox/programming/director-musices/buffer.midi"))))

;(defn fixed-track [n]
;  (proxy [javax.sound.midi.Track] []
;    (add [_] (throw (Exception. "track is immutable")))
;    (remove [_] (throw (Exception. "track is immutable")))
;    (get [i] (.get (nth (.getTracks @dynamic-sequence) n) i))
;    (size [] (.size (nth (.getTracks @dynamic-sequence) n)))
;    (ticks [] (.ticks (nth (.getTracks @dynamic-sequence) n)))))

;(def fixed-sequence 
;  (proxy [javax.sound.midi.Sequence] [0.0 240]
;    (createTrack [] (throw (Exception. "sequence is immutable")))
;    (deleteTrack [_] (throw (Exception. "sequence is immutable")))
;    (getDivisionType [] (.getDivisionType @dynamic-sequence))
;    (getMicrosecondLength [] (.getMicrosecondLength @dynamic-sequence))
;    (getPatchList [] (.getPatchList @dynamic-sequence))
;    (getResolution [] (.getResolution @dynamic-sequence))
;    (getTickLength [] (.getTickLength @dynamic-sequence))
;    (getTracks [] 
;      (into-array (map fixed-track (range (count (.getTracks @dynamic-sequence)))))
;      )))

(def sequencer (MidiSystem/getSequencer))
(.open sequencer)
( .addShutdownHook (Runtime/getRuntime) (Thread. #(.close sequencer)))
;(.setSequence sequencer fixed-sequence)

(defn start! []
  (.start sequencer))

(defn stop! []
  (.stop sequencer)
  (.setTickPosition sequencer 0))

(defn pause! []
  (.stop sequencer))

(defn update! []
;  (apply-current-rulepalette)
  (save-midi-to-path "buffer.midi")
  ;(reset! dynamic-sequence (MidiSystem/getSequence (file "buffer.midi")))
  (.setSequence sequencer (MidiSystem/getSequence (file "buffer.midi")))
  )

(def player-panel
  (ssw/toolbar :items [(ssw/action :icon (resource "icons/play.png")
                                   :handler (fn [_] (start!)))
                       (ssw/action :icon (resource "icons/pause.png")
                                   :handler (fn [_] (pause!)))
                       (ssw/action :icon (resource "icons/refresh.png")
                                   :handler (fn [_] (update!)))
                       :separator
                       (ssw/action :icon (resource "icons/stop.png")
                                   :handler (fn [_] (stop!)))]))

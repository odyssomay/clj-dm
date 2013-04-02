; Blatantly copied directly from simip
; https://github.com/odyssomay/simip

(ns director-musices.player
  (:use [clojure.java.io :only [resource file]])
  (:require (director-musices
              [global :as global]
              [util :as util])
            (director-musices.score
              [glue :as score-glue])
            [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]])
  (:import javax.sound.midi.MidiSystem))

(global/native!)

(declare update-if-update?)

(def sequencer (atom nil))
(def transmitter (atom nil))
(def output-device (atom nil))
(def receiver (atom nil))

(add-watch sequencer nil (fn [_ _ _ s] (reset! transmitter (.getTransmitter s))))

(defn sequencer-ready? []
  (and @sequencer (.isOpen @sequencer) (.getSequence @sequencer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Controls

(defn start! []
  (update-if-update?)
  (when (sequencer-ready?)
    (if (< (- (.getTickLength @sequencer) (.getTickPosition @sequencer)) 10)
      (.setTickPosition @sequencer 0))
    (.start @sequencer)
    true))

(defn stop! []
  (when (sequencer-ready?)
    (.stop @sequencer) 
    (.setTickPosition @sequencer 0)))

(defn pause! []
  (when (sequencer-ready?)
    (.stop @sequencer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File handling

(let [midi-file (atom nil)]
  (defn reload-midi-file []
    (when (and @midi-file @sequencer (.isOpen @sequencer))
      (.setSequence @sequencer (MidiSystem/getSequence @midi-file))
;      (start!)
      true))

  (defn open-midi-file [f]
    (reset! midi-file f)
    (reload-midi-file))

  (defn choose-midi-file []
    (when-let [f (ssw-chooser/choose-file :filters [["Midi Files" ["midi" "mid" "smf"]]])]
      (stop!)
      (open-midi-file f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Device

(defn open-sequencer [s]
  (try
    (reset! sequencer s)
    (if-not (.isOpen s) (.open s))
    (.addShutdownHook (Runtime/getRuntime) (Thread. #(.close s)))
    (catch javax.sound.midi.MidiUnavailableException _
      (ssw/alert "Midi device is unavailable")
      (System/exit 1))))

(defn open-output-device [s]
  (try
    (when @output-device (.close @output-device))
    (when @receiver (.close @receiver))
    (reset! output-device s)
    (let [r (.getReceiver @output-device)]
      (reset! receiver r)
      (.open s)
      (.setReceiver @transmitter r)
      (.addShutdownHook (Runtime/getRuntime) (Thread. #(.close r)))
      )
    (catch javax.sound.midi.MidiUnavailableException _
      (ssw/alert "Midi device is unavailable")
      (System/exit 1))))

(def get-sequencers
  (memoize 
    (fn []
      (->> 
        (MidiSystem/getMidiDeviceInfo)
        (map #(MidiSystem/getMidiDevice %))
        (filter #(isa? (class %) javax.sound.midi.Sequencer))))))

(def get-output-devices
  (memoize 
    (fn []
      (->> 
        (MidiSystem/getMidiDeviceInfo)
        (map #(MidiSystem/getMidiDevice %))
        (filter #(and (not= (.getMaxReceivers %) 0)
                      (not (isa? (class %) javax.sound.midi.Sequencer))))))))

(def get-synthesizers
  (memoize 
    (fn []
      (->> 
        (MidiSystem/getMidiDeviceInfo)
        (map #(MidiSystem/getMidiDevice %))
        (filter #(isa? (class %) javax.sound.midi.Synthesizer))))))

(let [cb (ssw/combobox :model (map #(.getName (.getDeviceInfo %)) (get-output-devices))
                       :border 10)]
  (defn set-midi-device [index]
    (if-not (= (.getSelectedIndex cb) index)
      (.setSelectedIndex cb index))
    (let [s (nth (get-output-devices) index)]
      (stop!)
      (open-output-device s)
      (reload-midi-file)))
  (defn choose-midi-device []
    (let [dialog (ssw/dialog :content cb
                             :type :plain
                             :option-type :ok-cancel
                             :modal? true
                             :success-fn (fn [& _] 
                                           (set-midi-device (.getSelectedIndex cb))))]
      (-> dialog ssw/pack! ssw/show!))))

;; =====
;; Init
;; =====
(open-sequencer (first (get-sequencers)))
(open-output-device (first (get-output-devices)))

;; =====
;; Update
;; =====
(defn update-player []
  (let [f (java.io.File. (util/tmp-dir) "buffer.midi")]
    (score-glue/save-midi-to-path (.getCanonicalPath f))
    (open-midi-file f)))

(let [update? (atom false)]
  (defn update-later! []
    (reset! update? true))
  
  (defn update-if-update? []
    (when @update?
      (update-player))))

(defn listen-to-position [f]
  (util/thread
    (loop []
      (Thread/sleep 50)
      (when-let [s @sequencer]
        (when (.isRunning s)
          (let [position (/ (.getTickPosition s)
                            (.getTickLength s))]
            (f position))))
      (recur))))

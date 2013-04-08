; Blatantly copied directly from simip
; https://github.com/odyssomay/simip

(ns director-musices.player
  (:use [clojure.java.io :only [resource file]])
  (:require (director-musices
              [global :as global]
              [util :as util])
            [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]])
  (:import javax.sound.midi.MidiSystem))

(global/native!)

(def sequencer (atom nil))
(def transmitter (atom nil))
(def output-device (atom nil))
(def receiver (atom nil))

(add-watch sequencer nil (fn [_ _ _ s] (reset! transmitter (.getTransmitter s))))

(defn sequencer-ready? []
  (and @sequencer (.isOpen @sequencer) (.getSequence @sequencer)))

(defn get-sequencer []
  (if (sequencer-ready?)
    @sequencer))

;; =====
;; Position listener
;; =====
(def position-listener (atom nil))

(defn fire-position-listener! [s]
  (when-let [f @position-listener]
    (let [position (/ (.getTickPosition s)
                      (.getTickLength s))]
      (f position))))

(util/thread
  (loop []
    (Thread/sleep 50)
    (when (sequencer-ready?)
      (let [s @sequencer]
        (when (.isRunning s)
          (fire-position-listener! s))))
    (recur)))

(defn listen-to-position [f]
  (reset! position-listener f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Controls

(defn start! []
  (when (sequencer-ready?)
    (if (< (- (.getTickLength @sequencer) (.getTickPosition @sequencer)) 10)
      (.setTickPosition @sequencer 0))
    (.start @sequencer)
    true))

(defn stop! []
  (when (sequencer-ready?)
    (let [s @sequencer]
      (.stop s)
      (.setTickPosition s 0)
      (fire-position-listener! s))))

(defn pause! []
  (when (sequencer-ready?)
    (let [s @sequencer]
      (.stop s)
      (fire-position-listener! s))))

(defn toggle-pause! []
  (when (sequencer-ready?)
    (if (.isRunning @sequencer)
      (pause!)
      (start!))))

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
;; Set position
;; =====
(defn position! [x]
  (when (sequencer-ready?)
    (let [s @sequencer
          x (max 0 (min 1 x))]
      (.setTickPosition s (long (* x (.getTickLength s))))
      (fire-position-listener! s))))

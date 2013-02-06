; Blatantly copied directly from simip
; https://github.com/odyssomay/simip

(ns director-musices.player
  (:use [director-musices.common-lisp.glue :only [save-midi-to-path]]
        [clojure.java.io :only [resource file]])
  (:require [seesaw 
             [core :as ssw]
             [chooser :as ssw-chooser]])
  (:import javax.sound.midi.MidiSystem))

(ssw/native!)

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
;; Position Control

(def position-is-updating? (atom false))
(def position-indicator (ssw/slider :min 0 :max 0))
(def position-indicator-label (ssw/label :text "0:0/0:0" :border 5))
(def indicator-panel (ssw/card-panel :items [[(ssw/border-panel :west position-indicator-label
                                                                :center position-indicator)
                                              "position"]
                                             [(ssw/progress-bar :indeterminate? true :border 10) "progress"]]))

(defn update-position-indicator-range []
  (when (sequencer-ready?)
    (reset! position-is-updating? true)
    (.setMaximum position-indicator (int (.getMicrosecondLength @sequencer)))
    (reset! position-is-updating? false)))

(def init-position-indicator
  (memoize 
    (fn []
      (.start
        (Thread. (fn [] 
                   (when (sequencer-ready?)
                     (reset! position-is-updating? true)
                     (.setValue position-indicator (int (.getMicrosecondPosition @sequencer)))
                     (reset! position-is-updating? false))
                   (Thread/sleep 100)
                   (recur))))
      (.addChangeListener position-indicator
        (reify javax.swing.event.ChangeListener
          (stateChanged [_ _]
            (if (and (not @position-is-updating?)
                     (sequencer-ready?))
              (.setMicrosecondPosition @sequencer (.getValue position-indicator))))))
      (.addChangeListener position-indicator
        (reify javax.swing.event.ChangeListener
          (stateChanged [_ e]
            (ssw/config! position-indicator-label :text
              (let [pos-in-seconds (/ (.getValue (.getSource e)) 1000000)
                    length-in-seconds (/ (.getMicrosecondLength @sequencer) 1000000)]
                (str (quot pos-in-seconds 60) 
                     ":" 
                     (int (mod pos-in-seconds 60))
                     "/"
                     (quot length-in-seconds 60)
                     ":"
                     (int (mod length-in-seconds 60))
                     ))))))
      )))

(defn show-position-indicator []
  (init-position-indicator)
  (update-position-indicator-range)
  (ssw/show-card! indicator-panel "position"))

(defn show-progress-indicator []
  (ssw/show-card! indicator-panel "progress"))

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
    (reload-midi-file)
    (show-position-indicator))

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
      (show-progress-indicator)
      (-> dialog ssw/pack! ssw/show!)
      (show-position-indicator))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main UI

(def controls-panel
  (let [pla (ssw/action :icon (resource "icons/play.png") :tip "play")
        paa (ssw/action :icon (resource "icons/pause.png") :tip "pause")
        plb (ssw/button :action pla)
        pab (ssw/button :action paa)]
    (ssw/config! pla :handler
      (fn [_]
        (if-not (start!)
          (choose-midi-file))
        (.setFocusable plb false)
        (.setFocusable pab true)
        (.requestFocusInWindow pab)))
    (ssw/config! paa :handler
      (fn [_]
        (pause!)
        (.setFocusable plb true)
        (.setFocusable pab false)
        (.requestFocusInWindow plb)))
    (ssw/toolbar 
      :floatable? false
      :items 
      [plb pab
       :separator
       (ssw/action :icon (resource "icons/stop.png")
                   :handler (fn [_] (stop!))
                   :tip "stop")
       :separator
;       (ssw/action :icon (resource "icons/document.png")
;                   :handler (fn [_] (choose-midi-file))
;                   :tip "Open midi file")
;       (ssw/action :icon (resource "icons/document_refresh.png")
;                   :handler (fn [_] (if-not (reload-midi-file)
;                                      (choose-midi-file)))
;                   :tip "Reload selected file")
       (ssw/action :icon (resource "icons/gear.png")
                   :handler (fn [_] (choose-midi-device))
                   :tip "Select midi device")])))

(defn disable-focus [content]
  (let [cs (.getComponents content)]
    (if (> (count cs) 0)
      (dorun (map disable-focus cs))
      (if-not (= (.getToolTipText content) "play")
        (.setFocusable content false)))))

(def player-panel
  (let [content (ssw/border-panel :center controls-panel
                                  :south indicator-panel)]
    (open-sequencer (first (get-sequencers)))
    (open-output-device (first (get-output-devices)))
    (disable-focus content)
    content
    ))

(defn update-player []
  (save-midi-to-path "buffer.midi")
  (open-midi-file (file "buffer.midi"))
  )


; Blatantly copied directly from simip
; https://github.com/odyssomay/simip

(ns director-musices.player
  (:use [clojure.java.io :only [resource file]])
  (:require (director-musices
              [global :as global]
              [util :as util])
            (director-musices.score
              [global :as score-global]
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
;; Position Control

(def position-is-updating? (atom false))
(def position-indicator (ssw/slider :min 0 :max 0))
(def position-indicator-label (ssw/label :text "00:00/00:00" :border 5))
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
            (when (and (not @position-is-updating?)
                     (sequencer-ready?))
              (.setMicrosecondPosition @sequencer (.getValue position-indicator))))))
      (.addChangeListener position-indicator
        (reify javax.swing.event.ChangeListener
          (stateChanged [_ e]
            (ssw/config! position-indicator-label :text
              (let [pos-in-seconds (/ (.getValue (.getSource e)) 1000000)
                    length-in-seconds (/ (.getMicrosecondLength @sequencer) 1000000)]
                (format "%02d:%02d/%02d:%02d"
                        (int (quot pos-in-seconds 60))
                        (int (mod pos-in-seconds 60))
                        (int (quot length-in-seconds 60))
                        (int (mod length-in-seconds 60))
                        )
                ; (str (quot pos-in-seconds 60) 
                ;      ":" 
                ;      (int (mod pos-in-seconds 60))
                ;      "/"
                ;      (quot length-in-seconds 60)
                ;      ":"
                ;      (int (mod length-in-seconds 60))
                ;      )
                )))))
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
      :floatable? true
      :items
      [plb pab
       :separator
       (ssw/action :icon (resource "icons/stop.png")
                   :handler (fn [_] (stop!))
                   :tip "stop")
       :separator
       (ssw/action :icon (resource "icons/gear.png")
                   :handler (fn [_] (choose-midi-device))
                   :tip "Select midi device")
       :separator
       "scale"
       (ssw/slider :value 100
                   :min 30
                   :max 300
                   :listen [:change (fn [e]
                                      (score-global/scale!
                                        (double (/ (.getValue (.getSource e))
                                                   100)))
                                      )])
       ]
      )))

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
  (.addChangeListener position-indicator
    (reify javax.swing.event.ChangeListener
      (stateChanged [_ e]
        (f (/ (.getValue position-indicator)
              (.getMaximum position-indicator)))))))
        ; (let [s (.getSource e)
        ;       m (.getMaximum s)
        ;       v (.getValue s)]
        ;   (f (/ v m)))))))

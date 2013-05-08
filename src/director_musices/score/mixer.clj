(ns director-musices.score.mixer
  (:require (director-musices
              [global :as dm-global]
              [player :as player])
            (director-musices.score
              [glue :as glue])
            (seesaw
              [core :as ssw]
              [mig :as ssw-mig]))
  (:import javax.sound.midi.ShortMessage))

(def mig ssw-mig/mig-panel)

; All properties:
; "trackname" "midi-channel"
; "midi-initial-volume" "midi-initial-program"
; "midi-bank-msb" "midi-bank-lsb"
; "midi-pan" "midi-reverb" 
; "synth" 
; "instrument-type"
; "track-delay"

(defn listen-and-set-property [id c type property reload-later!]
  (let [value (case type
                :string ssw/text
                :bool #(.isSelected %)
                ssw/selection)
        listen-property (case type
                          :string :document
                          :synth :selection
                          :bool :selection
                          :midi-program-list :selection
                          :change)
        update
        (fn [value]
          (glue/set-track-property id property value))]
    (ssw/listen c listen-property
      (fn [& _]
        (update (value c))
        (reload-later!)))))

(defn mixer-view [id reload-later!]
  (let [prop (fn [property default]
               (let [v (glue/get-track-property id property)]
                 (if (nil? v) default v)))
        trackname (ssw/text (prop "trackname" id))
        active? (ssw/checkbox :selected? (prop "active-p" true))
        volume-display (ssw/progress-bar :orientation :vertical
                                         :value 0
                                         :min 0 :max 127)
        volume-control (ssw/slider :orientation :vertical
                                   :value (prop "midi-initial-volume" 0)
                                   :min -39 :max 0)
        
        pan (ssw/slider :min 0 :max 127
                        :value (prop "midi-pan" 64) :major-tick-spacing 32
                        :minor-tick-spacing 8
                        :snap-to-ticks? true)
        synth (let [c (ssw/combobox :id :synth :model (glue/get-defined-synths))]
                (.setSelectedIndex c (prop "synth" 0))
                c)
        program (let [c (ssw/combobox :id :program-list
                                      :model (glue/get-track-synth-program-list id))]
                  (.setSelectedIndex c (dec (prop "midi-initial-program" 1)))
                  c)
        p (mig :items [[trackname "span, growx"]
                       ["Active"]
                       [active? "wrap, align right"]
                       [volume-display "align right, growy"]
                       [volume-control "h 200!, wrap"]
                       [pan "span, w 100!"]
                       [synth "span, growx, w 100!"]
                       [program "span, growx, w 100!"]]
               :constraints [])]
    (doseq [[c type property] [[trackname :string "trackname"]
                               [active? :bool "active-p"]
                               [volume-control :slider "midi-initial-volume"]
                               [pan :slider "midi-pan"]
                               [synth :synth "synth"]
                               [program :midi-program-list "midi-initial-program"]]]
      (listen-and-set-property id c type property reload-later!))
    (ssw/listen synth :selection
      (fn [_]
        (doto program
          (ssw/config! :model (glue/get-track-synth-program-list id))
          (.setSelectedIndex (dec (prop "midi-initial-program" 1))))))
    p))

(defn mixer [reload-later!]
  (let [views (for [id (range (glue/get-track-count))]
                [(mixer-view id reload-later!)])
        p (mig :items
               (interleave views
                           (repeatedly #(vector (ssw/separator :orientation :vertical)
                                                "growy")))
               :constraints ["insets 0"])]
    p))

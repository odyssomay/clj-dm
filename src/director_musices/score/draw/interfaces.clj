(ns director-musices.score.draw.interfaces)

(defprotocol scoreProperties
  (setScale [this scale] )
  (setScaleX [this scale-x])
  (setScaleFromHeight [this height])
  (setNotes [this notes])
  (getOptionsAtom [this]))

(defprotocol scoreGraphProperties
  (setHeight [this height])
  (setTitle [this title]))
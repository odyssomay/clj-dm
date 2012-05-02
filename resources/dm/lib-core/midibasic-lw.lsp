;;;-*-Mode: LISP; Package: DM -*-
;;
;; 0609/Anders Friberg

(in-package :dm)


;; -------------------
;;   MIDI-WRITE-LIST
;; -------------------
;; 

(defun midi-write-list (list time)
   (if (get-dm-var 'to-midi-file?)
       (midifile-write-list list time) ;defined in MIDIFileOutput
     (error "MIDI-WRITE-LIST: only saving to midi file is implemented")
     ))

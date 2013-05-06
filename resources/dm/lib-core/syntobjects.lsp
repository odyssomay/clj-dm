;;;-*-Mode: LISP; Package: DM -*-
;;
;; ***********************************************************
;;  Definition of synt objects used by the playing functions
;;  This is used to play a synt via MIDI using the internal
;;  parameters in DM
;; ***********************************************************
;;
;;/Anders Friberg 
;; 2000.08.25 Roberto Bresin added definition of roland-pma5 
;; 2000.09.08 Roberto Bresin added definition of technics-sx-p30
;; 2000.09.25 af added definition of roland-1010
;; 2000.10.18 af added *defined-synths*, synth-name-to-symbol, synth-symbol-to-name
;;            new synth objects is updated only in this file - not exactly - also in the dialog list in musicdialog-win.lsp
;; 2001.07.12 rb added definition of roland-1010 orchestral II expansion board
;; 2002.10.04 Stephane Letz (Grame) note-on add a sequence in the MidiShare sequence
;; 2002.12.14/af fixed the new midishare stuff using a mixure of midi commands and midi streams (free midi list in midishare)
;; 2003.05.01/af changed velocity scaling of all synt objects using sl-to-vel method
;; 061003/af added bank select
;; 061018/af added reverb and pan
;; 110416/af added Yamaha Clavinova CLP-370, all synt def now only in this file
;; 121202/af added Yamaha P90
;; 130502/af added accent analysis midi output

(in-package :dm)

;;used for translating name to synth object
(defvar *defined-synths*)
(setq *defined-synths* 
      '(("Pinnacle" . synt-pinnacle)
        ("SBlive" . synt-SBlive)
        ("Roland-A90" . synt-Roland-A90)
        ("Roland-PMA5" . synt-Roland-PMA5)
        ("Roland-1010" . synt-Roland-1010)
        ("Roland-1010-OrchII" . synt-Roland-1010-OrchII)
        ("Roland-1010-OrchIIB" . synt-Roland-1010-OrchIIB)
        ("Kontakt2-Piano" . synt-Kontakt2-piano)
        ("Kontakt2-wind" . synt-Kontakt2-wind)
        ("Technics-SX-P30" . synt-Technics-SX-P30)
        ("Yamaha-Clavinova-CLP370" . synt-Yamaha-Clavinova-CLP370)
        ("Yamaha-Disklavier-2" . synt-Yamaha-Disklavier-2)
        ("Yamaha-P90" . synt-Yamaha-P90)       
        ("Proteus" . synt-Proteus)
        ;("SampleCell" . synt-SampleCell)
        ;("S3000" . synt-S3000)
        ;("FZ1" . synt-FZ1)
        ;("fb01" . synt-fb01)
        ;("dx21" . synt-dx21)
        ;("Musse" . synt-Musse)
        ;("Generator" . synt-Generator)
        ))

;;used for defining menu items in music dialog
(defvar *defined-synth-names*)
(setq *defined-synth-names* 
      '("Pinnacle"
        "SBlive"
        "Roland-A90"
        "Roland-PMA5"
        "Roland-1010"
        "Roland-1010-OrchII"
        "Roland-1010-OrchIIB"
        "Kontakt2-Piano"
        "Kontakt2-wind"
        "Technics-SX-P30"
        "Yamaha-Clavinova-CLP370"
        "Yamaha-Disklavier-2"
        "Yamaha-P90"
        "Proteus"
        ;"SampleCell"
        ;"S3000"
        ;"FZ1"
        ;"fb01"
        ;"dx21"
        ;"Musse"
        ;"Generator"
        ))

(defun synth-name-to-symbol (name)
  (cdr (assoc name *defined-synths* :test 'string-equal)) )

(defun synth-symbol-to-name (name)
  (car (rassoc name *defined-synths*)) )

;used in the mac popup
(defun synth-symbol-to-index (name)
  (dotimes (i (length *defined-synths*))
    (if (equal (cdr (nth i *defined-synths*)) name)
      (return (1+ i)) )))

(defun make-synth (name)
 (apply (synth-name-to-symbol name) nil) )

;---------the basic synt structure------------------------------

(defclass synt ()
  (
   (channel :initarg :channel :initform 1 :accessor channel)
   (dc-range :initarg :dc-range :initform 2 :accessor dc-range)     ;nr of semitones 
   (vel-mean :initarg :vel-mean
             :initform (+ (get-dm-var 'vel-mean) 0) :accessor vel-mean) ;mean value for velocity which sl will be added to
   (program-list :initarg :program-list :initform *program-list-generic* :accessor program-list)
        ))

#|
(defmethod note-on ((synt synt) key vel time)
   ;(print-ll "key " key " vel " vel)
   (midi-write-list (list (logior #x90 (1- (channel synt)))
                      (+ key (get-dm-var 'transpose-shift)) vel )
     time) )
|#

(defmethod note-on ((synt synt) key vel time)
  ;(print-ll synt "key " key " vel " vel)
  (if (get-dm-var 'to-midi-file?)
    (midifile-write-list (list (logior #x90 (1- (channel synt))) key vel) time) 
    (midi-add-seq (get-dm-var 'midishare-seq)
                  (key-on :pitch key :vel vel :chan (1- (channel synt))
                          :date time
                          :port (get-DM-var 'midishare-port)))
    ))

#|
(defmethod note-on-sl ((synt synt) key sl time)
   (setq sl (round (+ (vel-mean synt) (* sl 2.0 (get-dm-var 'vel-scale)))))
   (note-on synt key
     (cond ((< sl 1) (warn "Underflow in velocity, time = ~A s, key = ~A, synt = ~S" (/ time 1000.0) key synt) 1)
           ((> sl 127) (warn "Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defun test (synt key sl time)
 (warn "Underflow in velocity, time = ~A s, key = ~A, synt = ~S" (/ time 1000.0) key synt)
  )



(defmethod note-on-sl ((synt synt) key sl time)
   (let ((vel (sl-to-vel synt sl)))
   (note-on synt key
     (cond ((< vel 1) (warn "Synt: Underflow in velocity") 1)
           ((> vel 127) (warn "Synt: Overflow in velocity") 127)
           (t vel) )
     time )))

;;return the maximum sl value that gives vel = 127
(defmethod get-max-sl ((synt synt))
  (let ((sl 0.0))
    (while t
      (if (> (sl-to-vel synt sl) 127)
          (return (- sl 0.1)) )
      (incf sl 0.1)
      )))
    

;according to midi spec
(defmethod all-notes-off ((synt synt) time)
   ;;;     (for (channel 0 1 15) old macro syntax
  ;(print time)
   (dotimes (channel 16)
      ;;(midi-write-list (list (logior #xB0 channel) 123 0) time) ;all notes off
      ;;(midi-write-list (list (logior #xB0 channel) 121 0) time) ;reset all controllers
      (midi-send-im (get-dm-var 'midishare-refnum) (ctrl-change :ctrl 123 :val 0 :chan channel :port (get-DM-var 'midishare-port)))
      (midi-send-im (get-dm-var 'midishare-refnum) (ctrl-change :ctrl 121 :val 0 :chan channel :port (get-DM-var 'midishare-port)))
      (setf (channel synt) (1+ channel))
      (set-dc synt 0 time)
      (set-va synt 0 time) )
   (set-dm-var 'to-midi-file? nil)
   (set-dm-var 'last-time 0)
   )

(defmethod all-notes-off ((score score) time)
  (dolist (track (track-list score))
    (if (synth track)
      (all-notes-off (synth track) time)
      (all-notes-off (make-synth (get-dm-var 'play-synth-name-default)) time))
    ))


;(defun foo-on () (note-on synt 50 70 0))

;(defun foo-off () (note-on synt 50 0 0))

(defmethod set-channel ((synt synt) nr)
   (if (or (> nr 16) (< nr 1))    ;not  0 < nr < 17 ?
      (error "set-channel: wrong channel number ~D" nr) )
  (setf (channel synt) nr) )


;-----------vibrato--------------

;vibrato amplitude
;va = 127 gives 12.5 percent vibrato
;sends amp as a high byte on control 1
#|
(defmethod set-va ((synt synt) va time)
   (midi-write-list (list (logior #xB0 (1- (channel synt))) 1 va) time) )

|#

 (defmethod set-va ((synt synt) va time)
   ;(print-ll "set va  " va)
   (if (get-dm-var 'to-midi-file?)
       (midifile-write-list (list (logior #xB0 (1- (channel synt))) 1 va) time)
       ;(midi-send-im (get-dm-var 'midishare-refnum) 
       ;  (ctrl-change :ctrl 1 :val va :chan (1- (channel synt)) :date time :port (get-DM-var 'midishare-port)))
       (midi-add-seq (get-dm-var 'midishare-seq)
                  (ctrl-change :ctrl 1 :val va :chan (1- (channel synt)) :date time :port (get-DM-var 'midishare-port)) )
       ))

;vibrato frequency
;sends vf as a high byte on control 6 
#|
(defmethod set-vf ((synt synt) vf time)
  ;(setf (vf synt) vf)
  (setq vf (truncate (/ (* 32 vf) 50.)))        ;scale for sisse 32/50
  (midi-write-list (list (logior #xB0 (1- (channel synt))) 6 vf)
  )
|#

(defmethod set-vf ((synt synt) vf time)
  ;(setf (vf synt) vf)
  (setq vf (truncate (/ (* 32 vf) 50.)))        ;scale for sisse 32/50
  (if (get-dm-var 'to-midi-file?)
      (midifile-write-list (list (logior #xB0 (1- (channel synt))) 6 vf) time)
    (midi-add-seq (get-dm-var 'midishare-seq) 
                (ctrl-change :ctrl 6 :val vf :chan (1- (channel synt)) ;;*****should be control 6???????
                             :date time :port (get-DM-var 'midishare-port))
                )))


;-----------program--------------

#|
;sends program change
(defmethod set-program ((synt synt) program time)
   ;(setf (program synt) program)
   (midi-write-list (list (logior #xC0 (1- (channel synt))) (1- program))
     time ))

(defmethod set-program ((synt synt) program time)  
   (if (get-dm-var 'to-midi-file?)
      (midifile-write-list (list (logior #xC0 (1- (channel synt))) (1- program))
        time )
      #+:mcl
      (midi-write-list (list (logior #xC0 (1- (channel synt))) (1- program)) time )
      #+:mswindows
      (midisendat (get-dm-var 'midishare-refnum) 
        (prog-change :pgm (1- program) :chan (1- (channel synt))
           :port (get-DM-var 'midishare-port)) time)
      ))
|#

(defmethod set-program ((synt synt) program time)  
  (if (get-dm-var 'to-midi-file?)
    (midifile-write-list (list (logior #xC0 (1- (channel synt))) (1- program)) time )
    (midi-add-seq (get-dm-var 'midishare-seq) 
                  (prog-change :pgm (1- program) :chan (1- (channel synt)) :date time :port (get-DM-var 'midishare-port)))
    ))

;-----------bank select------------
   
(defmethod set-bank-msb ((synt synt) bank-msb time)
  (if (or (< bank-msb 0)(> bank-msb 127))
      (warn "Synt: MIDI BankMSB outside range (0-127)")
    (if (get-dm-var 'to-midi-file?)
        (midifile-write-list (list (logior #xB0 (1- (channel synt))) 0 bank-msb) time )
      (warn "MIDI bank message not implemented in Midishare")
      )))

(defmethod set-bank-lsb ((synt synt) bank-lsb time)
  (if (or (< bank-lsb 0)(> bank-lsb 127))
      (warn "Synt: MIDI BankLSB outside range (0-127)")
    (if (get-dm-var 'to-midi-file?)
        (midifile-write-list (list (logior #xB0 (1- (channel synt))) 32 bank-lsb) time )
      (warn "MIDI bank message not implemented in Midishare")
      )))

;-----------pan select------------
   
   
(defmethod set-pan ((synt synt) pan time)
  (if (or (< pan 0)(> pan 127))
      (warn "Synt: MIDI pan outside range (0-127)")
    (if (get-dm-var 'to-midi-file?)
        (midifile-write-list (list (logior #xB0 (1- (channel synt))) 10 pan) time )
      (warn "MIDI pan message not implemented in Midishare")
      )))

;-----------reverb select------------
   
(defmethod set-reverb ((synt synt) reverb time)
  (if (or (< reverb 0)(> reverb 127))
      (warn "Synt: MIDI reverb outside range (0-127)")
    (if (get-dm-var 'to-midi-file?)
        (midifile-write-list (list (logior #xB0 (1- (channel synt))) 91 reverb) time )
      (warn "MIDI reverb message not implemented in Midishare")
      )))

;---------- accent salience midi output ------------
;accent-c = control change 16
;accent-m = control change 17
;accent-h = control change 18


(defmethod set-accent-c ((synt synt) accent-c time)
  (if (or (< accent-c 0)(> accent-c 127))
      (warn "Synt: MIDI control change (accent-c) outside range (0-127)")
    (if (get-dm-var 'to-midi-file?)
        (midifile-write-list (list (logior #xB0 (1- (channel synt))) 16 accent-c) time )
      (warn "MIDI accent message not implemented in Midishare")
      )))

(defmethod set-accent-m ((synt synt) accent-m time)
  (if (or (< accent-m 0)(> accent-m 127))
      (warn "Synt: MIDI control change (accent-m) outside range (0-127)")
    (if (get-dm-var 'to-midi-file?)
        (midifile-write-list (list (logior #xB0 (1- (channel synt))) 17 accent-m) time )
      (warn "MIDI accent-m message not implemented in Midishare")
      )))

(defmethod set-accent-h ((synt synt) accent-h time)
  (if (or (< accent-h 0)(> accent-h 127))
      (warn "Synt: MIDI control change (accent-h) outside range (0-127)")
    (if (get-dm-var 'to-midi-file?)
        (midifile-write-list (list (logior #xB0 (1- (channel synt))) 18 accent-h) time )
      (warn "MIDI accent-h message not implemented in Midishare")
      )))

;-----------volume-----------------

#| for testing
(defmethod set-vol ((synt synt) vol time)
   ;(setf (vol synt) vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;sends volume, -63.5 <= vol <= 0  [dB]
;-63.5 or less give zero amplitude
#|
(defmethod set-vol ((synt synt) vol time)
   ;(setf (vol synt) vol)
   (if (< vol -63.5) (setq vol -63.5))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (round (+ 127 (* 2 vol))))
     time )
   ;(print (round (+ 127 (* 2 vol))))
   )
|#

(defmethod set-vol ((synt synt) vol time)
  ;(setf (vol synt) vol)
  ; (print-ll "set volume  " vol  "  MIDI: " (round (+ 127 (* 2 vol))))
  (if (< vol -63.5) (setq vol -63.5))
  (if (get-dm-var 'to-midi-file?)
      (midifile-write-list 
       (list (logior #xB0 (1- (channel synt))) 7 (round (+ 127 (* 2 vol)))) time )
    (midi-add-seq (get-dm-var 'midishare-seq) 
                  (ctrl-change :ctrl 7 :val (round (+ 127 (* 2 vol))) :chan (1- (channel synt))
                               :date time :port (get-DM-var 'midishare-port)))
    ))

;-----------pitch bend----------------


#|
;pitchbend including low byte
(defmethod set-dc ((synt synt) dc time)
   ;(setf (dc synt) dc)
   (let ((value (+ 64 (/ (* 64.0 dc) (* 100 (dc-range synt))))))
      (midi-write-list (list (logior #xE0 (1- (channel synt)))
                         (logand #x7F (round (* 128 value)))
                         (truncate value))
        time )))
;pitchbend including low byte
(defmethod set-dc ((synt synt) dc time)
  ;(setf (dc synt) dc)
  (let ((value (/ (* 64.0 dc) (* 100 (dc-range synt)))))
    (print-ll "dc " dc " value " value " dc-range " (dc-range synt))
    (if (get-dm-var 'to-midi-file?)
      (midifile-write-list (list (logior #xE0 (1- (channel synt)))
                         (logand #x7F (round (* 128 value)))
                         (truncate value)) time)
      (midi-add-seq (get-dm-var 'midishare-seq) 
                    (pitch-bend :bend (round value) :chan (1- (channel synt))
                                :date time :port (get-DM-var 'midishare-port)))
      )))
|#


;fixed error in formula above
(defmethod set-dc ((synt synt) dc time)
  ;(setf (dc synt) dc)
  (let ((value (+ 64 (/ (* 64.0 dc) (* 100 (dc-range synt))))))
    ;(print-ll "dc " dc " value " value " dc-range " (dc-range synt))
    (if (get-dm-var 'to-midi-file?)
      (midifile-write-list (list (logior #xE0 (1- (channel synt)))
                         (logand #x7F (round (* 128 value)))
                         (truncate value)) time)
      (midi-add-seq (get-dm-var 'midishare-seq) 
                    (pitch-bend :bend (round value) :chan (1- (channel synt))
                                :date time :port (get-DM-var 'midishare-port)))
    )))


;sends pedal on/off
;controller 64
#|
(defmethod set-pedal-perf ((synt synt) value time)
   (if (not (or (=  value 1)(= value 0)))
      (error "wrong pedal value: ~A time: ~A" value time) )
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       64                     
       (case value (1 127) (0 0)))
     time ))
|#

(defmethod set-pedal-perf ((synt synt) value time)
  (if (not (or (= value 1)(= value 0)))
      (error "wrong pedal value: ~A time: ~A" value time) )
  (if (get-dm-var 'to-midi-file?)
      (midifile-write-list (list (logior #xB0 (1- (channel synt))) 64 (case value (1 127) (0 0)))
                           time )
    (midi-add-seq (get-dm-var 'midishare-seq) 
                  (ctrl-change :ctrl 64 :val (case value (1 127) (0 0)) 
                               :chan (1- (channel synt)) :date time :port (get-DM-var 'midishare-port)))
  
  ))

;for midifiles
;strings longer than 127 chars will be truncated
;changed to lyrics event/af 9710
(defmethod meta-event-text ((synt synt) string time)
  ;(print-ll "meta-event-text: "  string)
   (let ((len (length string))
         (str-list '()))
      (if (> len 127) (setq len 127))
      (dotimes (i len)
         (newr str-list (char-code (char string i))))
      (midi-write-list 
        (append (list #xFF #x05 len) str-list)
        time )))

;----------- MUSSE -----------------------------------------------------

(defclass synt-musse (synt)
    ((vel-mean :initarg :vel-mean
       :initform (+ (get-dm-var 'vel-mean) 30) :accessor vel-mean))
   )

(defun synt-musse () (make-instance 'synt-musse))

(defmethod print-object ((self synt-musse) stream)
  (format stream "(synt-musse)") )

#|
(defmethod note-on-sl ((synt synt-musse) key sl time)
   (setq sl (round (+ (vel-mean synt) (* sl 2.0 (get-dm-var 'vel-scale)))))
   (note-on synt key
     (cond ((< sl 1) (warn "Synt: Underflow in velocity") 1)
           ((> sl 127) (warn "Synt: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-musse) sl)
  (round (+ (vel-mean synt) (* sl 2.0 ))) )

;send cent * 64/50 (dc) as the high byte in pitch bend
;could be modified to send the low byte also
;cent range = +49, -50 cent
(defmethod set-dc ((synt synt-musse) dc time)
   ;(setf (dc synt) dc)
   (midi-write-list (list (logior #xE0 (1- (channel synt)))
                      0 (+ (round (* dc 1.28)) #x40) )
     time ))

;vibrato amplitude
;sends va as a high byte on control 1
;range: 0 - 200 cent
;not tested
(defmethod set-va ((synt synt-musse) va time)
   ;(setf (va synt) va)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 1 (round (* 0.635 va))) ;127/200
     time) )

;vibrato frequency
;sends vf as a high byte on control 23
;range: 0.1 - 25.5 Hz
;not tested
(defmethod set-vf ((synt synt-musse) vf time)
   ;(setf (vf synt) vf)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 23 (round (* 5 vf)))
     time) )


;----------- PROTEUS -----------------------------------------------------

;;measurements on Proteus/2 Orchestral

(defclass synt-proteus (synt) ( ))

(defun synt-proteus () (make-instance 'synt-proteus :program-list *program-list-proteus2*))

(defmethod print-object ((self synt-proteus) stream)
  (format stream "(synt-proteus)") )

;;velocity arrray for input -9 dB to +8.5 in 0.5 dB steps
;;assuming 0 db in the middle
(defvar velocity-array-proteus)
(setq velocity-array-proteus 
  (make-array
    '(36)
    :initial-contents
    '(3 4 7 8 10 12 13 15 17 20 22 26 29 34 40 45 52 60 70 80 88
      92 95 100 102 107 110 112 114 116 117 118 121 123 124 127) ))

;;note on with relative amplitude in dB
;;and using (get-dm-var 'vel-scale) factor (normally 1)
; assuming vel sens = 48 in proteus
#|
(defmethod note-on-sl ((synt synt-proteus) key sl time)
   (setq sl (* (get-dm-var 'vel-scale) sl))
   (note-on synt key
     (cond
           ((< sl -9) (warn "Proteus: Underflow in velocity") 1)
           ((> sl 8.5) (warn "Proteus: Overflow in velocity") 127)
           (t (aref velocity-array-proteus (round (* 2 (+ 9 sl))))) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-proteus) sl)
  (cond
   ((< sl -9) (warn "Proteus: Underflow in velocity") 1)
   ((> sl 8.5) (warn "Proteus: Overflow in velocity") 127)
   (t (aref velocity-array-proteus (round (* 2 (+ 9 sl))))) )
  )

;;return the maximum sl value that gives vel = 127
(defmethod get-max-sl ((synt synt-proteus))
   8.5)

;sends volume, -63.5 <= vol <= 0  [dB]
;-63.5 or less give zero amplitude
;approximation accurate down to -40 dB
;error approx ± 0.5 dB
(defmethod set-vol ((synt synt-proteus) vol time)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond ((< vol -63.5) 0)
             ((plusp vol)
              (warn "Proteus: Overflow in volume")
              127)
             ((> vol -0.7) 127) ;compensate error in formula
             (t (round  (* 132.4734 (expt 10 (* 0.0253 vol))))) ))
     time ))

;(defun foo (vol) (* 132.4734 (expt 10 (* 0.0253 vol))))

;----------- Turtle beach Pinnacle -----------------------------------------------------

(defclass synt-Pinnacle (synt) ())


(defun synt-Pinnacle () 
   (make-instance 'synt-Pinnacle :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-Pinnacle) stream)
  (format stream "(synt-Pinnacle)") )

;;; (defmethod print-object ((self synt-Pinnacle) stream)
;;;   (format stream "Pinnacle") )

#|
;for testing
(defmethod note-on-sl ((synt synt-pinnacle) key sl time)
   (note-on synt key sl time ))
|#

 ;;0.00202x3 + 0.08054x2 + 2.87369x + 47.14180
#|
(defmethod note-on-sl ((synt synt-pinnacle) key sl time)
   (setq sl (round (* (+ (* 0.00202 (expt sl 3)) 
                      (* 0.08054 (expt sl 2)) 
                      (* 2.87369 sl) 
                      47.14180)
                      (get-dm-var 'vel-scale))))
   (note-on synt key
     (cond ((< sl 1) (warn "Pinnacle: Underflow in velocity") 1)
           ((> sl 127) (warn "Pinnacle: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-pinnacle) sl)
   (round (+ (* 0.00202 (expt sl 3)) 
             (* 0.08054 (expt sl 2)) 
             (* 2.87369 sl) 
             47.14180)
          ))

#| for testing
(defmethod set-vol ((synt synt-pinnacle) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

 ;128.62037e0.05478x
;127.00000e0.05421x 
;ok for vol from 0 to -45 dB 
(defmethod set-vol ((synt synt-pinnacle) vol time)
   (setq vol (round  (* 127.0 (exp (* 0.05421 vol)))))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "Pinnacle: Underflow in volume") 0)
           ((> vol 127) (warn "Pinnacle: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (* 128.62037 (exp (* 0.05478 vol)))))
 |#
;----------- Sound Blaster Live -----------------------------------------------------

(defclass synt-SBlive (synt) ())


(defun synt-SBlive () 
   (make-instance 'synt-SBlive :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-SBlive) stream)
  (format stream "(synt-SBlive)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-SBlive) key sl time)
   (note-on synt key sl time ))
|#

#|
 ;;0.00002x4 + 0.0024x3 + 0.1008x2 + 3.2466x + 66.603
(defmethod note-on-sl ((synt synt-SBlive) key sl time)
   (setq sl (round (* (+ (* 0.00002 (expt sl 4))
                         (* 0.0024 (expt sl 3)) 
                         (* 0.1008 (expt sl 2)) 
                         (* 3.2466 sl) 
                         66.603)
                      (get-dm-var 'vel-scale))))
   (note-on synt key
     (cond ((< sl 1) (warn "SBlive: Underflow in velocity") 1)
           ((> sl 127) (warn "SBlive: Overflow in velocity") 127)
           (t sl) )
            time ))

;for testing
(defun sl-to-vel-sblive (sl)
    (+ (* 0.00002 (expt sl 4))
              (* 0.0024 (expt sl 3)) 
              (* 0.1008 (expt sl 2)) 
              (* 3.2466 sl) 
              66.603)
  )

;"inverts" the sl-to-vel function by searching for close fit
(defun find-vel-to-db (target-vel)
  (let ((vel 0.5)
        (db 0.0)
        (step 0.05)
        (limit 0.2) )
    (while (> (abs (- target-vel vel)) limit)
      (setq vel (vol-to-midi-volume-sblive db))
      (if (>  vel target-vel)
          (decf db step)
        (incf db step) )
       )
    (/ (round (* 100.0 db)) 100.0)
    ))

;print a list of all db values corresponding to velocity 1-127
(defun make-list-vel-to-db ()
  (loop for i from 1 to 127 do
        (princ " ") (princ (find-vel-to-db i))
        ))
|#

;the inverse of sl-to-vel function below
;better a method for each synth
(defun vel-to-sl-sblive (vel)
  (nth vel
       '(nil 
         -36.55 -35.95 -35.3 -34.65 -34.05 -33.4 -32.75 -32.1 -31.45 -30.8 -30.15 -29.5 -28.85 -28.15 -27.5 
         -26.8 -26.15 -25.45 -24.75 -24.1 -23.4 -22.7 -22.0 -21.3 -20.6 -19.95 -19.25 -18.55 -17.9 -17.2 -16.55 
         -15.9 -15.25 -14.65 -14.0 -13.4 -12.8 -12.2 -11.65 -11.05 -10.5 -10.0 -9.45 -8.95 -8.45 -7.95 -7.45 -7.0 
         -6.55 -6.1 -5.65 -5.25 -4.85 -4.4 -4.05 -3.65 -3.25 -2.9 -2.55 -2.2 -1.85 -1.5 -1.15 -0.85 -0.5 -0.2 0.15 
         0.45 0.75 1.05 1.3 1.6 1.9 2.15 2.4 2.7 2.95 3.2 3.45 3.7 3.95 4.2 4.4 4.65 4.8 5.1 5.25 5.55 5.75 5.9 6.2 
         6.4 6.6 6.8 7.0 7.2 7.4 7.6 7.7 7.95 8.15 8.35 8.45 8.7 8.8 9.05 9.15 9.4 9.5 9.75 9.9 10.0 10.25 10.4 10.55 
         10.65 10.8 11.05 11.2 11.35 11.5 11.65 11.8 11.95 12.1 12.15 12.3)
       ))

(defmethod sl-to-vel ((synt synt-SBlive) sl)
    (round (+ (* 0.00002 (expt sl 4))
              (* 0.0024 (expt sl 3)) 
              (* 0.1008 (expt sl 2)) 
              (* 3.2466 sl) 
              66.603)
              ))

;;return the maximum sl value that gives vel = 127
(defmethod get-max-sl ((synt synt-SBlive))
  (let ((sl 0.0))
    (while t
      (if (> (sl-to-vel synt sl) 127)
          (return (- sl 0.1)) )
      (incf sl 0.1)
      )))
    
#| for testing
(defmethod set-vol ((synt synt-SBlive) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;0.0002x3 + 0.0581x2 + 5.1593x + 124.52
(defmethod set-vol ((synt synt-SBlive) vol time)
   (setq vol (round  (+  (* 0.0002 (expt vol 3)) 
                         (* 0.0581 (expt vol 2)) 
                         (* 5.1593 vol) 
                         124.52)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "SBlive: Underflow in volume") 0)
           ((> vol 127) (warn "SBlive: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
;for testing
(defun vol-to-midi-volume-sblive (vol) 
  (+  (* 0.0002 (expt vol 3)) 
             (* 0.0581 (expt vol 2)) 
             (* 5.1593 vol) 
             124.52))
|#

;the inverse of set-vol function below
;better a method for each synth
(defun midi-volume-to-vol-sblive (vel)
  (nth vel
       '( -38.85 -38.2 -37.6 -37.0 -36.4 -35.8 -35.25 -34.7 -34.2 -33.65 -33.15 -32.65 -32.15 -31.65
             -31.2 -30.75 -30.25 -29.8 -29.4 -28.95 -28.5 -28.1 -27.65 -27.25 -26.85 -26.45 -26.05
             -25.65 -25.3 -24.9 -24.5 -24.15 -23.8 -23.4 -23.05 -22.7 -22.35 -22.0 -21.65 -21.35 -21.0 
             -20.65 -20.35 -20.0 -19.7 -19.35 -19.05 -18.75 -18.4 -18.1 -17.8 -17.5 -17.2 -16.9 -16.6 -16.3 
             -16.0 -15.75 -15.45 -15.15 -14.9 -14.6 -14.35 -14.05 -13.8 -13.5 -13.25 -13.0 -12.7 -12.45 -12.2 
             -11.95 -11.65 -11.4 -11.15 -10.9 -10.65 -10.4 -10.15 -9.9 -9.65 -9.4 -9.2 -8.95 -8.7 -8.45 -8.25 -8.0 
             -7.75 -7.55 -7.3 -7.05 -6.85 -6.6 -6.4 -6.15 -5.95 -5.7 -5.5 -5.3 -5.05 -4.85 -4.55 -4.4 -4.2 -4.0 
             -3.7 -3.55 -3.35 -3.15 -2.95 -2.75 -2.45 -2.25 -2.1 -1.9 -1.7 -1.5 -1.3 -1.1 -0.9 -0.7 -0.45 -0.25 
             -0.15 0.05 0.3 0.5
             )
       ))


;----------- Roland A90 -----------------------------------------------------

(defclass synt-roland-a90 (synt) ())


(defun synt-roland-a90 () 
   (make-instance 'synt-roland-a90 :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-roland-a90) stream)
  (format stream "(synt-roland-a90)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-roland-a90) key sl time)
   (note-on synt key sl time ))
|#

#|
 ;;y = 0.0014x3 + 0.0984x2 + 3.447x + 62.724
(defmethod note-on-sl ((synt synt-roland-a90) key sl time)
   (setq sl (round (* (+ (* 0.0014 (expt sl 3)) 
                         (* 0.0984 (expt sl 2)) 
                         (* 3.447 sl) 
                         62.724)
                      (get-dm-var 'vel-scale))))
   (note-on synt key
     (cond ((< sl 1) (warn "synt-roland-a90: Underflow in velocity") 1)
           ((> sl 127) (warn "synt-roland-a90: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-roland-a90) sl)
   (round (+ (* 0.0014 (expt sl 3)) 
             (* 0.0984 (expt sl 2)) 
             (* 3.447 sl) 
             62.724)
          ))

#| for testing
(defun foo (sl)
   (round (+ (* 0.0014 (expt sl 3)) 
             (* 0.0984 (expt sl 2)) 
             (* 3.447 sl) 
             62.724)
          ))
|#

#| for testing
(defmethod set-vol ((synt synt-roland-a90) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;y = 0.0027x3 + 0.2322x2 + 7.9045x + 125.7
(defmethod set-vol ((synt synt-roland-a90) vol time)
   (setq vol (round  (+  (* 0.0027 (expt vol 3)) 
                         (* 0.2322 (expt vol 2)) 
                         (* 7.9045 vol) 
                         125.7)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-roland-a90: Underflow in volume") 0)
           ((> vol 127) (warn "synt-roland-a90: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (+  (* 0.0027 (expt vol 3)) 
                         (* 0.2322 (expt vol 2)) 
                         (* 7.9045 vol) 
                         125.7)))
 |#

;----------- Roland PMA-5 -----------------------------------------------------

(defclass synt-roland-pma5 (synt) ())


(defun synt-roland-pma5 () 
   (make-instance 'synt-roland-pma5 :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-roland-pma5) stream)
  (format stream "(synt-roland-pma5)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-roland-pma5) key sl time)
   (note-on synt key sl time ))
|#

#|
;;y = 0.0011x3 + 0.1192x2 + 4.5149x + 62.764
(defmethod note-on-sl ((synt synt-roland-pma5) key sl time)
   (setq sl (round (* (+ (* 0.0011 (expt sl 3)) 
                         (* 0.1192 (expt sl 2)) 
                         (* 4.5149 sl) 
                         62.764)
                      (get-dm-var 'vel-scale))))
   (note-on synt key
     (cond ((< sl 1) (warn "synt-roland-pma5: Underflow in velocity") 1)
           ((> sl 127) (warn "synt-roland-pma5: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-roland-pma5) sl)
   (round (+ (* 0.0011 (expt sl 3)) 
             (* 0.1192 (expt sl 2)) 
             (* 4.5149 sl) 
             62.764)
          ))
 #| for testing
(defun foo (sl)
   (round (+ (* 0.0011 (expt sl 3)) 
             (* 0.1192 (expt sl 2)) 
             (* 4.5149 sl) 
             62.764)
          ))
|#

 #| for testing
(defmethod set-vol ((synt synt-roland-pma5) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;;y = 0.0013x3 + 0.1652x2 + 7.3794x + 125.09
(defmethod set-vol ((synt synt-roland-pma5) vol time)
   (setq vol (round  (+  (* 0.0013 (expt vol 3)) 
                         (* 0.1652 (expt vol 2)) 
                         (* 7.3794 vol) 
                         125.09)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-roland-pma5: Underflow in volume") 0)
           ((> vol 127) (warn "synt-roland-pma5: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (+  (* 0.0013 (expt vol 3)) 
                         (* 0.1652 (expt vol 2)) 
                         (* 7.3794 vol) 
                         125.09)))
 |#

;----------- Roland 1010 -----------------------------------------------------

(defclass synt-roland-1010 (synt) ())

(defun synt-roland-1010 () 
   (make-instance 'synt-roland-1010 :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-roland-1010) stream)
  (format stream "(synt-roland-1010)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-roland-1010) key sl time)
   (note-on synt key sl time ))
|#
#|
;;y = 0.0017x3 + 0.1193x2 + 4.0845x + 63.883
(defmethod note-on-sl ((synt synt-roland-1010) key sl time)
   (setq sl (round (* (+ (* 0.0017 (expt sl 3)) 
                         (* 0.1193 (expt sl 2)) 
                         (* 4.0845 sl) 
                         63.883)
                      (get-dm-var 'vel-scale))))
   (note-on synt key
     (cond ((< sl 1) (warn "synt-roland-1010: Underflow in velocity") 1)
           ((> sl 127) (warn "synt-roland-1010: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-roland-1010)  sl)
   (round (+ (* 0.0017 (expt sl 3)) 
             (* 0.1193 (expt sl 2)) 
             (* 4.0845 sl) 
             63.883)
          ))

 #| for testing
(defmethod set-vol ((synt synt-roland-1010) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;;y = 0.0013x3 + 0.1652x2 + 7.3794x + 125.09
;;y = 0.0023x3 + 0.1969x2 + 7.2826x + 126.16
(defmethod set-vol ((synt synt-roland-1010) vol time)
   (setq vol (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                         126.16)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-roland-1010: Underflow in volume") 0)
           ((> vol 127) (warn "synt-roland-1010: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                         126.16)))
 |#


;----------- Roland 1010 Orchestral II-----------------------------------------------------

(defclass synt-roland-1010-orchII (synt) ())

(defclass synt-roland-1010-orchIIB (synt-roland-1010-orchII) ())

(defun synt-roland-1010-orchII () 
   (make-instance 'synt-roland-1010-orchII :program-list *program-list-roland-1010-orchII*))

(defun synt-roland-1010-orchIIB () 
  (make-instance 'synt-roland-1010-orchIIB :program-list *program-list-roland-1010-orchIIB*))

(defmethod print-object ((self synt-roland-1010-orchII) stream)
  (format stream "(synt-roland-1010-orchII)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-roland-1010-orchII) key sl time)
   (note-on synt key sl time ))
|#
#|
;;y = 0.0017x3 + 0.1193x2 + 4.0845x + 63.883
(defmethod note-on-sl ((synt synt-roland-1010-orchII) key sl time)
   (setq sl (round (* (+ (* 0.0017 (expt sl 3)) 
                         (* 0.1193 (expt sl 2)) 
                         (* 4.0845 sl) 
                         63.883)
                      (get-dm-var 'vel-scale))))
   (note-on synt key
     (cond ((< sl 1) (warn "synt-roland-1010-orchII: Underflow in velocity") 1)
           ((> sl 127) (warn "synt-roland-1010-orchII: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-roland-1010-orchII) sl)
   (round (+ (* 0.0017 (expt sl 3)) 
             (* 0.1193 (expt sl 2)) 
             (* 4.0845 sl) 
             63.883)
          ))

#| for testing
(defmethod set-vol ((synt synt-roland-1010-orchII) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;;y = 0.0013x3 + 0.1652x2 + 7.3794x + 125.09
;;y = 0.0023x3 + 0.1969x2 + 7.2826x + 126.16
(defmethod set-vol ((synt synt-roland-1010-orchII) vol time)
   (setq vol (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                         126.16)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-roland-1010-orchII: Underflow in volume") 0)
           ((> vol 127) (warn "synt-roland-1010-orchII: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                         126.16)))
 |#

;----------- Kontakt2 piano (Steinway Light)-----------------------------------------------------

(defclass synt-kontakt2-piano (synt) ())

(defun synt-kontakt2-piano () 
   (make-instance 'synt-kontakt2-piano :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-kontakt2-piano) stream)
  (format stream "(synt-kontakt2-piano)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-kontakt2-piano) key sl time)
   (note-on synt key sl time ))

(defun foo (sl)
   (round (+ (* 0.00001 (expt sl 4))
                        (* 0.0008 (expt sl 3)) 
                        (* 0.0402 (expt sl 2)) 
                        (* 2.6899 sl) 
                        62.025)
          ))
|#

;;y = 1E-05x4 + 0.0008x3 + 0.0402x2 + 2.6899x + 62.025
(defmethod sl-to-vel ((synt synt-kontakt2-piano) sl)
   (round (+ (* 0.00001 (expt sl 4))
                        (* 0.0008 (expt sl 3)) 
                        (* 0.0402 (expt sl 2)) 
                        (* 2.6899 sl) 
                        62.025)
          ))

#| for testing
(defmethod set-vol ((synt synt-kontakt2-piano) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;******WARNING VOLUME NOT IMPLEMENTED FOR KONTAKT2 PIANO******************

;;y = 0.0013x3 + 0.1652x2 + 7.3794x + 125.09
;;y = 0.0023x3 + 0.1969x2 + 7.2826x + 126.16
(defmethod set-vol ((synt synt-kontakt2-piano) vol time)
   (setq vol (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                        126.16)))
  (warn "wrong midi volume translation in Kontakt2-Piano (no measurements yet)")
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-kontakt2-piano: Underflow in volume") 0)
           ((> vol 127) (warn "synt-kontakt2-piano: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                         126.16)))
|#


;----------- Kontakt2 wind instruments (french horn, flute, trumpet, all measured from sustain samples)-----------------------------------------------------

(defclass synt-kontakt2-wind (synt) ())

(defun synt-kontakt2-wind () 
   (make-instance 'synt-kontakt2-wind :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-kontakt2-wind) stream)
  (format stream "(synt-kontakt2-wind)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-kontakt2-wind) key sl time)
   (note-on synt key sl time ))

(defun foo (sl)
   (round (+ (* -0.000001 (expt sl 4))
                        (* -0.0024 (expt sl 3)) 
                        (* 0.0002 (expt sl 2)) 
                        (* 4.0532 sl) 
                        64.003)
          ))
|#

;;y = -1E-06x4 - 0.0024x3 + 0.0002x2 + 4.0532x + 64.003
(defmethod sl-to-vel ((synt synt-kontakt2-wind) sl)
   (round (+ (* -0.000001 (expt sl 4))
                        (* -0.0024 (expt sl 3)) 
                        (* 0.0002 (expt sl 2)) 
                        (* 4.0532 sl) 
                        64.003)
          ))

#| for testing
(defmethod set-vol ((synt synt-kontakt2-wind) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;******WARNING VOLUME NOT IMPLEMENTED FOR KONTAKT2 wind******************

;;y = 0.0013x3 + 0.1652x2 + 7.3794x + 125.09
;;y = 0.0023x3 + 0.1969x2 + 7.2826x + 126.16
(defmethod set-vol ((synt synt-kontakt2-wind) vol time)
   (setq vol (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                        126.16)))
  (warn "wrong midi volume translation in Kontakt2-wind (no measurements yet)")
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-kontakt2-wind: Underflow in volume") 0)
           ((> vol 127) (warn "synt-kontakt2-wind: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (+  (* 0.0023 (expt vol 3)) 
                         (* 0.1969 (expt vol 2)) 
                         (* 7.2826 vol) 
                         126.16)))
|#

;send attack time as control 73 - according to midi spec **** just nu ctrl 0 så att bt patch funkar ****
;transformation according to pDM braintuning exp4 patch 2008
(defmethod set-at ((synt synt-kontakt2-wind) at time)
  (setq at (round (if (>= at 94)
                      (- (* 14.162 (log at)) 9.3269)
                    (if (>= at 6)
                        (- (* 12.735 (log at)) 3.3324)
                      (+  (* -0.5026 (expt at 2)) 
                         (* 6.0492 at) 
                         0.1938 )))))
  ;(print-ll "set at  " at)
  (if (get-dm-var 'to-midi-file?)
      (midifile-write-list (list (logior #xB0 (1- (channel synt))) 0 at) time)
    ;(midi-send-im (get-dm-var 'midishare-refnum) 
    ;  (ctrl-change :ctrl 1 :val va :chan (1- (channel synt)) :date time :port (get-DM-var 'midishare-port)))
    (midi-add-seq (get-dm-var 'midishare-seq)
                  (ctrl-change :ctrl 73 :val at :chan (1- (channel synt)) :date time :port (get-DM-var 'midishare-port)) )
    ))


;----------- Technics SX-P30 -----------------------------------------------------

(defclass synt-technics-sx-p30 (synt) ())


(defun synt-technics-sx-p30 () 
   (make-instance 'synt-technics-sx-p30 :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-technics-sx-p30) stream)
  (format stream "(synt-technics-sx-p30)") )

#|
;for testing
(defmethod note-on-sl ((synt synt-technics-sx-p30) key sl time)
   (note-on synt key sl time ))
|#
#|
;; y = 0.0002x4 + 0.0015x3 + 0.0267x2 + 3.8031x + 64.867
(defmethod note-on-sl ((synt synt-technics-sx-p30) key sl time)
  (setq sl (round (*  (+ (* 0.0002 (expt sl 4))
                         (* 0.0015 (expt sl 3)) 
                         (* 0.0267 (expt sl 2)) 
                         (* 3.8031 sl) 
                         64.867)
                      (get-dm-var 'vel-scale))))
   (note-on synt key
     (cond ((< sl 1) (warn "synt-technics-sx-p30: Underflow in velocity") 1)
           ((> sl 127) (warn "synt-technics-sx-p30: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-technics-sx-p30) sl)
  (round (+ (* 0.0002 (expt sl 4))
            (* 0.0015 (expt sl 3)) 
            (* 0.0267 (expt sl 2)) 
            (* 3.8031 sl) 
            64.867) ))
 #| for testing
(defun foo (sl)
  (round (+ (* 0.0002 (expt sl 4))
            (* 0.0015 (expt sl 3)) 
            (* 0.0267 (expt sl 2)) 
            (* 3.8031 sl) 
            64.867) ))
|#

 #| for testing
(defmethod set-vol ((synt synt-technics-sx-p30) vol time)
   (print vol)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7  vol)
     time ))
|#

;;****totally out to lunch* -check formulas****
;; y = -69046x3 + 25857x2 - 2507.8x + 110.46
(defmethod set-vol ((synt synt-technics-sx-p30) vol time)
   (setq vol (round  (+  (* -69046 (expt vol 3)) 
                         (* 25857 (expt vol 2)) 
                         (* -2507.8 vol) 
                         110.46)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-technics-sx-p30: Underflow in volume") 0)
           ((> vol 127) (warn "synt-technics-sx-p30: Overflow in volume") 127)
           (t vol) ))
     time ))
#|
(defun foo (vol) (round  (+  (* -69046 (expt vol 3)) 
                         (* 25857 (expt vol 2)) 
                         (* -2507.8 vol) 
                         110.46)))
 |#

;----------- Yamaha Clavinova CLP370 (Graz Erica/Parncutt) -----------------------------------------------------

(defclass synt-yamaha-clavinova-clp370 (synt) ())

(defun synt-yamaha-clavinova-clp370 () 
   (make-instance 'synt-yamaha-clavinova-clp370 :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-yamaha-clavinova-clp370) stream)
  (format stream "(synt-yamaha-clavinova-clp370)") )

;;y=0.0547x2 + 3.4035x + 64.695s
(defmethod sl-to-vel ((synt synt-yamaha-clavinova-clp370) sl)
  (round (+ (* 0.0547 (expt sl 2)) 
            (* 3.4035 sl) 
            64.0) ))

;; y = 0.0797x2 + 5.9317x + 127.82
(defmethod set-vol ((synt synt-yamaha-clavinova-clp370) vol time)
   (setq vol (round  (+  (* 0.0797 (expt vol 2)) 
                         (* 5.9317 vol) 
                         127.0)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-yamaha-clavinova-clp370 Underflow in volume") 0)
           ((> vol 127) (warn "synt-yamaha-clavinova-clp370 Overflow in volume") 127)
           (t vol) ))
     time ))

#| for testing
(defun foo (sl)
  (round (+ (* 0.0547 (expt sl 2)) 
            (* 3.4035 sl) 
            64.0) ) )
(defun foo (vol) (round  (+  (* 0.0797 (expt vol 2)) 
                         (* 5.9317 vol) 
                         127.0)))
 |#

;----------- Yamaha P90 -----------------------------------------------------

(defclass synt-yamaha-P90 (synt) ())

(defun synt-yamaha-P90 () 
   (make-instance 'synt-yamaha-P90 :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-yamaha-P90) stream)
  (format stream "(synt-yamaha-P90)") )

;;y = 0.0554x2 + 3.3468x + 63.927
(defmethod sl-to-vel ((synt synt-yamaha-P90) sl)
  (round (+ (* 0.0554 (expt sl 2)) 
            (* 3.3468 sl) 
            64.0) ))

;; y = 0.002x3 + 0.1802x2 + 7.1447x + 126.53
(defmethod set-vol ((synt synt-yamaha-P90) vol time)
   (setq vol (round  (+  (* 0.002 (expt vol 3))
                         (* 0.1802 (expt vol 2)) 
                         (* 7.1447 vol) 
                         127.0)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond
           ((< vol 0) (warn "synt-yamaha-P90 Underflow in volume") 0)
           ((> vol 127) (warn "synt-yamaha-P90 Overflow in volume") 127)
           (t vol) ))
     time ))

#| for testing
(defun foo (sl)
  (round (+ (* 0.0554 (expt sl 2)) 
            (* 3.3468 sl) 
            64.0) ) )
(defun foo (vol) (round  (+  (*  0.002 (expt vol 3))
                             (* 0.1802 (expt vol 2)) 
                         (* 7.1447 vol) 
                         127.0)))
 |#


;----------- Yamaha Disklavier II (Bresin, Goebl) -----------------------------------------------------
;; Measured from Disklavier II in Uppsalla
;; Goebl, W., & Bresin, R. (2003). Measurement and reproduction accuracy of computer-controlled grand pianos.
;; Journal of the Acoustical Society of America, 114(4), 2273-2283.

(defclass synt-yamaha-disklavier-2 (synt) ())

(defun synt-yamaha-disklavier-2 () 
   (make-instance 'synt-yamaha-disklavier-2 :program-list *program-list-general-midi*))

(defmethod print-object ((self synt-yamaha-disklavier-2) stream)
  (format stream "(synt-yamaha-disklavier-2)") )

;;y=0.0423x2 + 2.7998x + 63.836s
(defmethod sl-to-vel ((synt synt-yamaha-disklavier-2) sl)
  (round (+ (* 0.0423 (expt sl 2)) 
            (* 2.7998 sl) 
            63.836) ))

;; volume not implemented/measured

#| for testing
(defun foo (sl)
  (round (+ (* 0.0423 (expt sl 2)) 
            (* 2.7998 sl) 
            63.836) ) )
 |#

;----------- Generator software synthesizer -----------------------------------------------------
;

(defclass synt-generator (synt) ())


(defun synt-generator () (make-instance 'synt-generator))

(defmethod print-object ((self synt-generator) stream)
  (format stream "(synt-generator)") )

#|
;;sound level
;range: -20 to 20 dB
(defmethod note-on-sl ((synt synt-generator) key sl time)
   (setq sl (round (+ (vel-mean synt) (* 3.175 sl (get-dm-var 'vel-scale)))))
   (note-on synt key
     (cond ((< sl 1) (warn "synt-generator: Underflow in velocity") 1)
           ((> sl 127) (warn "synt-generator: Overflow in velocity") 127)
           (t sl) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-generator)  sl )
  (round (+ (vel-mean synt) (* 3.175 sl))) )


;vibrato amplitude
;sends va as a high byte on control 1
;range: 0 - 200 cent (+-)
;not tested
(defmethod set-va ((synt synt-generator) va time)
   (setq va (round (* 0.635 va))) ;127/200
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 1
       (cond
           ((< va 0) (warn "Generator: Underflow in va") 0)
           ((> va 127) (warn "Generator: Overflow in va") 127)
           (t va) ))
     time) )

;vibrato frequency
;sends vf as a high byte on control 23
;range: 2-12 Hz
;midi value = 127/(12-2)*f1 - 127*2/(12-2)
(defmethod set-vf ((synt synt-generator) vf time)
   (setq vf (round (- (* 12.7 vf) 25.4)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 23 
       (cond
           ((< vf 0) (warn "Generator: Underflow in vf") 0)
           ((> vf 127) (warn "Generator: Overflow in vf") 127)
           (t vf) ))
     time) )

;formant frequency f1
;range 200-1100 Hz
;midi value = 127/(1100-200)*f1 - 127*200/(1100-200)
;controller 11
(defmethod set-f1 ((synt synt-generator) f1 time)
   (setq f1 (round (- (* 0.1411111 f1) 28.222222)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 11 
       (cond
           ((< f1 0) (warn "Generator: Underflow in f1") 0)
           ((> f1 127) (warn "Generator: Overflow in f1") 127)
           (t f1) ))
     time) )

;formant frequency f2
;range 300-3000 Hz
;midi value = 127/(3000-300)*f1 - 127*300/(3000-300)
;controller 12
(defmethod set-f2 ((synt synt-generator) f2 time)
   (setq f2 (round (- (* 0.047037037 f2) 14.11111111)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 12 
       (cond
           ((< f2 0) (warn "Generator: Underflow in f2") 0)
           ((> f2 127) (warn "Generator: Overflow in f2") 127)
           (t f2) ))
     time) )

;formant frequency f3
;range 1500-5000 Hz
;midi value = 127/(5000-1500)*f1 - 127*1500/(5000-1500)
;controller 13
(defmethod set-f3 ((synt synt-generator) f3 time)
   (setq f3 (round (- (* 0.036285714 f3) 54.42857143)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 13 
       (cond
           ((< f3 0) (warn "Generator: Underflow in f3") 0)
           ((> f3 127) (warn "Generator: Overflow in f3") 127)
           (t f3) ))
     time) )

;formant frequency f4
;range 1600-6000 Hz
;midi value = 127/(6000-1600)*f1 - 127*1600/(6000-1600)
;controller 14
(defmethod set-f4 ((synt synt-generator) f1 time)
   (setq f1 (round (- (* 0.027608695 f1) 44.173913)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 14 
       (cond
           ((< f1 0) (warn "Generator: Underflow in f4") 0)
           ((> f1 127) (warn "Generator: Overflow in f4") 127)
           (t f1) ))
     time) )

;formant frequency f5
;range 1800-8000 Hz
;midi value = 127/(8000-1800)*f1 - 127*1800/(8000-1800)
;controller 15
(defmethod set-f5 ((synt synt-generator) f1 time)
   (setq f1 (round (- (* 0.02048387 f1) 36.87096774)))
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt))) 15 
       (cond
           ((< f1 0) (warn "Generator: Underflow in f5") 0)
           ((> f1 127) (warn "Generator: Overflow in f5") 127)
           (t f1) ))
     time) )


;----------- AKAI S3000 -----------------------------------------------------

;;measurements on AKAI S3000

(defclass synt-S3000 (synt)
    (
     ;(dc-range :initarg :dc-range :initform 2 :accessor dc-range)     ;nr of semitones
     ;(va-scale :initarg :va-scale :initform 100)   ;in percent: 100 means do nothing
     ;(vf-scale :initarg :vf-scale :initform 100)
     ))

;(defun new-S3000 () (make-instance 'synt-S3000))

(defun synt-S3000 () (make-instance 'synt-S3000))

(defmethod print-object ((self synt-S3000) stream)
  (format stream "(synt-S3000)") )

;;note on with relative amplitude in dB
;;and using (get-dm-var 'vel-scale) factor (normally 1)
; assuming vel sens = 20 in S3000 (factory setting)
#|
(defmethod note-on-sl ((synt synt-S3000) key sl time)
   (setq sl (* (get-dm-var 'vel-scale) sl))
   (note-on synt key
     (cond
           ((< sl -12) (warn "S3000: Underflow in velocity") 1)
           ((> sl 11.8) (warn "S3000: Overflow in velocity") 127)
           (t (round (+ 64 (* 5.3317 sl)))) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-S3000) sl)
   (round (+ 64 (* 5.3317 sl))) )

;sends volume
;range: -45 <= vol <= 0  [dB]
;-45 or less give zero amplitude
;approximation accurate down to -43 dB
;error approx ± 0.1 dB
(defmethod set-vol ((synt synt-S3000) vol time)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (cond ((<= vol -45) 0)
             ((plusp vol)
              (warn "S3000: Overflow in volume")
              127)
             (t (round  (* 126.3649 (expt 10 (* 0.0497 vol))))) ))
     time ))

;(defun foo (vol) (round  (* 126.3649 (expt 10 (* 0.0497 vol)))))


;----------- SampleCell -----------------------------------------------------

(defclass synt-SampleCell (synt)
    (
     ;(dc-range :initarg :dc-range :initform 1 :accessor dc-range)     ;nr of semitones
     ;(va-scale :initarg :va-scale :initform 100)   ;in percent: 100 means do nothing
     ;(vf-scale :initarg :vf-scale :initform 100)
     ))

;(defun new-SampleCell () (make-instance 'synt-SampleCell))

(defun synt-SampleCell () (make-instance 'synt-SampleCell :dc-range 1))

(defmethod print-object ((self synt-SampleCell) stream)
  (format stream "(synt-SampleCell)") )

;;note on with relative amplitude in dB
;;and using vel-scale factor (normally 1)
; assuming vel sens = 45 in SC
#|
(defmethod note-on-sl ((synt synt-SampleCell) key sl time)
   (setq sl (round (+ (vel-mean synt) 
                       (* sl  3.0 (get-dm-var 'vel-scale)))) )
   (note-on synt key
     (cond
           ((> sl 127)(warn "SampleCell: Overflow in velocity") 127)
           ((< sl 1)(warn "SampleCell: Underflow in velocity") 1)
           (t sl) )
            time ))
|#
(defmethod sl-to-vel ((synt synt-SampleCell) sl)
   (round (+ (vel-mean synt) 
             (* sl 3.0)
             )))

(defmethod all-notes-off ((synt synt-SampleCell) time)
   (print "all-notes-off SC")
   (dotimes (channel 16)
      (setf (channel synt) (1+ channel))
      ;(for (i 1 1 127)
      ;     (note-on synt i 10 time) )
      (dotimes (i 128)
         (note-on synt i 0 time) )
      (set-dc synt 0 time)
      (set-va synt 0 time) 
      (set-dm-var 'to-midi-file? nil)
      (set-dm-var 'last-time 0)
))


;vibrato amp as aftertouch
;range 0 <= va <= 87 cent
;mididata = va * 127/87, with va in +- cent deviation 
(defmethod set-va ((synt synt-SampleCell) va time)
   (midi-write-list 
     (list (logior #xD0 (1- (channel synt)))
       (cond ((< va 0) 0)
             ((> va 87) 127)
             (t (round (* 1.46 va))) ))
     time ))


#|
;for testing
(defmethod set-va ((synt synt-SampleCell) va time)
   (midi-write-list 
     (list (logior #xD0 (1- (channel synt)))
       (round (* 1.0 va)))
     time ))
|#

;vibrato freq as mod wheel
;range: vf = 1 to 11.5 Hz
;SampleCell generates only these frequencies:
;11.5  8.7  6.9  5.8  5.0  4.3  3.8  3.5  3.1  2.9  2.7  2.5 etc
(defmethod set-vf ((synt synt-SampleCell) vf time)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       1
       (round (+ -5.613 (* 11.8647 vf))) )
     time ))

;sends volume, -63.5 <= vol <= 0  [dB]
;-63.5 or less give zero amplitude
;approximation accurate down to -40 dB
(defmethod set-vol ((synt synt-SampleCell) vol time)
   (midi-write-list 
     (list (logior #xB0 (1- (channel synt)))
       7                     
       (if (< vol -63.5) 0 (round  (* 126.6381 (expt 10 (* 0.0226 vol))))) )
     time ))

;(defun foo (vol) (* 126.6381 (expt 10 (* 0.0226 vol))))

;----------- SISYFOS -----------------------------------------------------

           

;----------- FZ1 -----------------------------------------------------

;the same as the synt structure
;then the pitchbend will be wrong????
(defclass synt-fz1 (synt) ())

;(defun new-fz1 () (make-instance 'synt-fz1))

(defun synt-fz1 () (make-instance 'synt-fz1))

(defmethod print-object ((self synt-fz1) stream)
  (format stream "(synt-fz1)") )


;(defmethod note-on ((synt synt-fz1) key vel time)
; (midi-write-list (list (logior #x90 (1- (channel synt)))
;                        (+ key 12) vel)
;                  time) )

;;note on with relative amplitude in dB
;;and using vel-scale factor (normally 1)
;; vel = 0.31 * da0 * (get-dm-var 'vel-scale)
#|
(defmethod note-on-sl ((synt synt-fz1) key sl time)
   (note-on synt key
            (round (+ (vel-mean synt) (* sl 3.1 (get-dm-var 'vel-scale)))) time ))
|#

(defmethod sl-to-vel ((synt synt-fz1) sl)
     (round (+ (vel-mean synt) (* sl 3.1 ))) )

(defmethod all-notes-off ((synt synt-fz1) time)
   ;   (print "fz1 notoff")
   (dotimes (channel 16)
      (setf (channel synt) (1+ channel))  ; it's from 1 to 16, not 0 to 15
      (dotimes (i 11)
         (note-on synt (+ 74 i) 10 time) )
      (dotimes (i 11)
         (note-on synt (+ 74 i) 0 time) )
      (set-dc synt 0 time)
      (set-va synt 0 time)
      (set-dm-var 'to-midi-file? nil)
      (set-dm-var 'last-time 0)
))


;----------- FB01 -----------------------------------------------------

(defclass synt-fb01 (synt)
    (
     ;(dc-range :initarg :dc-range :initform 1 :accessor dc-range)     ;nr of semitones
     (va-scale :initarg :va-scale :initform 100 :accessor va-scale)   ;in percent: 100 means do nothing
     (vf-scale :initarg :vf-scale :initform 100 :accessor vf-scale)
     (sys-channel :initarg :sys-channel :initform 1 :accessor sys-channel)
     ))

;(defun new-fb01 () (make-instance 'synt-fb01))

(defun synt-fb01 () (make-instance 'synt-fb01 :dc-range 1))

(defmethod print-object ((self synt-fb01) stream)
  (format stream "(synt-fb01)") )


;;note on with relative amplitude in dB
;;and using vel-scale factor (normally 1)
;;aproximately working using oboe, basson, flute, PorganR, PorgR2 sound
;;the smallest step in FB01 is 0.75 dB
;;not linear in the vel = 64 region
;; the scaling of 0.36 derived using the file test-da0.ap with a range of +- 5 dB
;; and vel-mean = 90
#|
(defmethod note-on-sl ((synt synt-fb01) key sl time)
   (note-on synt key
     (+ (slot-value synt 'vel-mean)
        (round (* 3.6 (get-dm-var 'vel-scale) sl)) )
            time ))
|#

(defmethod sl-to-vel ((synt synt-fb01) sl)
     (+ (slot-value synt 'vel-mean)
        (round (* 3.6 sl)) ))

#|
(defun foo (da0)
   (print (/ (* da0 (get-dm-var 'vel-scale)) 7.5))
   (case (round (/ (* da0 (get-dm-var 'vel-scale)) 7.5))
     (0 58)(1 74)(-1 50)(2 82)(-2 42)(3 90)(-3 34)(4 98)(-4 26)
     (5 106)(-5 18)(6 114)(-6 10)(7 122)(-7 1)(8 126)
     (t (print "error in fb01:note-on-da0: over/underflow")) ))
|#

;sends program change as config change
(defmethod set-program ((synt synt-fb01) program time)
   ;(setf (slot-value synt 'program) program)
   (set-sys-par synt #x22 (1- program) time) )

;send cent as the high byte in pitch bend
;cent range = pitch-range given in semitones
;TEST OK for +/-100 cent
;(defmethod set-dc ((synt synt-fb01) dc time)
; (set-dc synt (truncate (/ (* 64.0 dc) (* 100 (dc-range synt))))    ;scale
;                time )
; (setf (slot-value synt 'dc) dc) )               ;put the cent value again in struct


;vibrato amplitude **** not right scaled *************
;va = 127 gives (12.5 percent vibrato)*va-scale
;sends amp as a high byte on control 1
(defmethod set-va ((synt synt-fb01) va time)
   (call-next-method synt (truncate (/ (* va (va-scale synt)) 100.0)) time)    ;scale
   ;(setf (va synt) va)    ;put the va value again in struct
   )

;vibrato frequency **** not right scaled **********
;sends vf as system parameter
;LFO speed also in other places 
#|
(defmethod set-vf ((synt synt-fb01) vf time)
   ; (setf (vf synt) vf)
   (slot-value 'set-sys-par synt #x09 (truncate (/ (* vf (vf-scale synt)) 100.0)) time) )
|#
;do nothing for the moment
(defmethod set-vf ((synt synt-fb01) vf time)
   (declare (ignore time))
   )
   ; (setf (slot-value synt 'vf) vf)

;;riset = risetime; platt = plateautime; dsustl = sustainlevel from top
;;sustt = sustaintime; relr = release rate
;;in ms, dB and dB/s
;;will also change som other unwanted parameters as well
;;which are not possible to isolate
;;op1adr = #x28; op2adr = #x20; op3adr = #x18; op4adr = #x10; 

#|  **** funkar ej med CL varfšr mapc ?
(defmethod set-env ((synt synt-fb01) riset platt dsustl sustt relr time)
   (let 
        ((arval (round (- 25.8079 (* 2.9367 (log riset)))))
         (slval 1)
         (d1rval (round (- 23.6202 (* 2.8217 (log platt)))))
         (d2rval (if (plusp dsustl)
                    0
                    (round (+ 0.8929 (* 2.9195 (log (/ (- dsustl) (* 0.001 sustt)))))) ))
         (rrval (round (+ -0.0024 (* 1.446 (log (- relr))))))
         (opadrlist '(#x28))
         )
      ;(print "syntval: ar " arval " sl " slval " d1r " d1rval " d2r " d2rval " rr " rrval )
      (mapc '(lambda (opadr)
               ;           (print "opadr " opadr)
               (set-voice-par synt (+ opadr 4) arval time)
               (set-voice-par synt (+ opadr 5) d1rval time)
               (set-voice-par synt (+ opadr 6) d2rval time)
               (set-voice-par synt (+ opadr 7)
                 (logior (ash slval 4) rrval) time) )
        opadrlist )))
|#

(defmethod set-env ((synt synt-fb01) riset platt dsustl sustt relr time)
   (let 
        ((arval (round (- 25.8079 (* 2.9367 (log riset)))))
         (slval 1)
         (d1rval (round (- 23.6202 (* 2.8217 (log platt)))))
         (d2rval (if (plusp dsustl)
                    0
                    (round (+ 0.8929 (* 2.9195 (log (/ (- dsustl) (* 0.001 sustt)))))) ))
         (rrval (round (+ -0.0024 (* 1.446 (log (- relr))))))
         (opadr #x28)
         )
      ;(print "syntval: ar " arval " sl " slval " d1r " d1rval " d2r " d2rval " rr " rrval )
      (set-voice-par synt (+ opadr 4) arval time)
      (set-voice-par synt (+ opadr 5) d1rval time)
      (set-voice-par synt (+ opadr 6) d2rval time)
      (set-voice-par synt (+ opadr 7)
        (logior (ash slval 4) rrval) time) ))


;(defun foo ()(set-env-rulle fb01 50 30 10 200 50 t0))

(defmethod set-config-par ((synt synt-fb01) par data time)
   (midi-write-list
     (list #xf0 #x43
       (logior #x10 (1- (channel synt)))
       #x15 par data  #xf7)
     time ))

(defmethod set-voice-par ((synt synt-fb01) par data time)
   (setq par (+ par #x40))           ;offset
   (midi-write-list
     (list #xf0 #x43
       (logior #x10 (1- (channel synt)))
       #x15 par (logand data #xf) (truncate (/ data 16.0))  #xf7)
     time ))

(defmethod set-sys-par ((synt synt-fb01) par data time)
   (midi-write-list
     (list #xf0 #x43 #x75 (1- (sys-channel synt))
       #x10 par data  #xf7)
     time ))

#|
(defmethod set-config-name ((synt synt-fb01) name time)
   (setq name (catenate name "         "))
   (dotimes (i 8)
      (set-sys-par synt i (chrnth i name) time) ))

(defun config-name (name) (slot-value 'set-config-name fb01 name t0))

(defun config (nr) (slot-value 'set-sys-par fb01 #x22 (1- nr) t0))

(defun config-par (par data)  (slot-value 'set-config-par fb01 par data t0))

(defun sys-par (par data)  (slot-value 'set-sys-par fb01 par data t0))
(defun vpar (par data)  (slot-value 'set-voice-par fb01 par data t0))
|#

;----------- DX21 -----------------------------------------------------

(defclass synt-dx21 (synt)
    (
     ;(dc-range :initarg :dc-range :initform 1 :accessor dc-range)     ;nr of semitones
     (va-scale :initarg :va-scale :initform 100)   ;in percent: 100 means do nothing
     (vf-scale :initarg :vf-scale :initform 50)
     ))

;(defun new-dx21 () (make-instance 'synt-dx21))

(defun synt-dx21 () (make-instance 'synt-dx21 :dc-range 1))

(defmethod print-object ((self synt-dx21) stream)
  (format stream "(synt-dx21)") )


;send cent as the high byte in pitch bend
;cent range = pitch-range given in semitones
;(defmethod set-dc ((synt synt-dx21) dc time)
; (set-dc synt (truncate (/ (* 128.0 dc) (* 100 (dc-range synt))))    ;scale
;                time )
; (setf (slot-value synt 'dc) dc) )               ;put the cent value again in struct


;vibrato amplitude
;va = 127 gives (12.5 percent vibrato)*va-scale
;sends amp as a high byte on control 1
(defmethod set-va ((synt synt-dx21) va time)
   (call-next-method synt (truncate (/ (* va (va-scale synt)) 100.0)) time)    ;scale
   ;(setf (va synt) va)
   )               ;put the va value again in struct

;vibrato frequency
;sends vf as parameter #54
;( defmethod set-vf ((synt synt-dx21) vf time)
; (setf (vf synt) vf)
; (set-par synt 54 (truncate (/ (* vf (vf-scale synt)) 100.0)) time) )

(defmethod set-vf ((synt synt-dx21) vf time)
   (setq vf (/ (float vf) 10)) ;frequency  in Hz
   (let ((lfo-speed (round (+ 10.6292
                              (* 4.181 vf)
                              (- (* 0.1018 (* vf vf)))
                              (* 0.001 (* vf vf vf)) ))))
      ;(setf (vf synt) vf)
      (set-par synt 54 lfo-speed time) ))

;(defun set-vf (vf) (set-vf dx21 vf t0))

;set all the attack-rates
(defmethod set-ar ((synt synt-dx21) ar time)
   (set-par synt 39 ar time)
   (set-par synt 26 ar time)
   (set-par synt 13 ar time)
   (set-par synt 0  ar time) )

;(defun set-ar (ar) (set-ar dx21 ar t0))


;set a parameter (parameter change, p 48)
;dx21 must be in edit mode (can also be set)
(defmethod set-par ((synt synt-dx21) par data time)
   ;(until (midi-check-output-size 10))
   (midi-write-list
     (list #xf0 #x43
       (logior #x10 (1- (channel synt)))
       #x12 par data  #xf7)
     time ))

;(defun set-par (par data) (set-par dx21 par data t0) )




;--------------------------------------------------------------
; PROGRAM NUMBER LISTS
;--------------------------------------------------------------

(defvar *program-list-general-midi*
  '(
"1 Acou Grand Piano"
"2 Bright Aco Piano"
"3 Elec Grand Piano"
"4 Honky-tonk Piano"
"5 RhodesPiano"
"6 ChorusPiano"
"7 Harpschord"
"8 Clavinet"
"9 Celesta"
"10 Glockenspiel"
"11 Music Box"
"12 Vibraphone"
"13 Marimba"
"14 Xylophone"
"15 TubularBells"
"16 Dulcimer"
"17 Hamnd Organ"
"18 Perc Organ"
"19 Rock Organ"
"20 ChurchOrgan"
"21 Reed Organ"
"22 Accordion"
"23 Harmonica"
"24 Tango Acordn"
"25 Nylon Guitar"
"26 SteelStrGuitar"
"27 Jazz Guitar"
"28 CleanE.Guitar"
"29 Mute E.Guitar"
"30 OvrdrivGuitar"
"31 DistortGuitar"
"32 Harmonics"
"33 Acou Bass"
"34 FingerE.Bass"
"35 PickedE.Bass"
"36 FretlesBass"
    "37 Slap Bass 1"
    "38 Slap Bass 2"
"39 Synth Bass1"
"40 Synth Bass2"
"41 Violin"
"42 Viola"
"43 Cello"
"44 Contrabass"
"45 Trem Strings"
"46 Pizz Strings"
"47 OrchHarp"
"48 Timpani"
"49 Str Ensmb 1"
"50 Str Ensmb 2"
"51 Synth Str 1"
"52 Synth Str 2"
"53 Choir Aahs"
"54 Voice Oohs"
"55 Synth Voice"
"56 Orchestra Hit"
"57 Trumpet"
"58 Trombone"
"59 Tuba"
"60 Mute Trumpet"
"61 French Horn"
"62 Brass Section"
"63 SynthBrass1"
"64 SynthBrass2"
"65 SopranoSax"
"66 Alto Sax"
"67 Tenor Sax"
"68 Bari Sax"
"69 Oboe"
"70 EnglshHorn"
"71 Bassoon"
    "72 Clarinet"
"73 Piccolo"
"74 Flute"
"75 Recorder"
"76 Pan Flute"
"77 BottleBlow"
"78 Shakuhachi"
"79 Whistle"
"80 Ocarina"
"81 Square Wave"
"82 SawTooth"
"83 Caliope"
"84 Chiff Lead"
"85 Charang"
"86 SoloSynthVox"
"87 Brite Saw"
"88 Brass&Lead"
"89 FantasaPad"
"90 Warm Pad"
"91 Poly Synth Pad"
"92 Space Vox Pad"
"93 Bow Glass Pad"
"94 Metal Pad"
"95 Halo Pad"
"96 Sweep Pad"
"97 Ice Rain"
"98 SoundTrack"
"99 Crystal"
"100 Atmosphere"
"101 Brightness"
    "102 Goblin"
"103 Echo Drops"
"104 Star Theme"
"105 Sitar"
"106 Banjo"
"107 Shamisen"
"108 Koto"
"109 Kalimba"
"110 Bag Pipe"
"111 Fiddle"
"112 Shanai"
"113 Tinkle Bell"
"114 Agogo"
"115 Steel Drums"
"116 Woodblock"
"117 Taiko Drum"
"118 Melodic Tom"
"119 Synth Drum"
"120 RevrsCymbal"
"121 GtrFretNoise"
"122 BreathNoise"
"123 Sea Shore"
"124 Bird Tweet"
"125 Telephone Ring"
"126 Helicopter"
"127 Applause"
"128 Gun Shot"
    ))

(defvar *program-list-generic*
  '(
"1" "2" "3" "4" "5" "6" "7" "8" "9" "10"
"11" "12" "13" "14" "15" "16" "17" "18" "19" "20"
"21" "22" "23" "24" "25"
"26"
"27"
"28"
"29"
"30"
"31"
"32"
"33" "34"
"35"
"36"
"37"
"38"
"39"
"40"
"41"
"42"
"43"
"44"
"45"
"46"
"47"
"48"
"49"
"50"
"51"
"52"
"53"
"54"
"55"
"56"
"57"
"58"
"59" "60"
"61"
"62"
"63"
"64"
"65"
"66"
"67"
"68"
"69"
"70"
"71"
"72"
"73"
"74"
"75"
"76"
"77"
"78"
"79"
"80"
"81"
"82"
"83" "84" "85"
"86"
"87"
"88"
"89"
"90"
"91"
"92"
"93"
"94"
"95"
"96"
"97" "98"
"99"
"100"
"101"
"102"
"103"
"104"
"105"
"106"
"107"
"108"
"109"
"110"
"111"
"112"
"113"
"114"
"115"
"116"
"117"
"118"
"119"
"120"
"121"
"122"
"123"
"124"
"125"
"126"
"127"
"128"
    ))


(defvar *program-list-proteus2*
  '(
"1 Solo Cello"
"2 Solo Viola"
"3 Solo Violin"
"4 Quartet"
"5 SoloChamber"
"6 Arco Basses"
"7 Arco Celli"
"8 Arco Violas"
"9 Arco Violins"
"10 Marcato 1"
"11 Marcato 2"
"12 Legato Str"
"13 Concerto"
"14 Pizz Basses"
"15 Pizz Celli"
"16 Pizz Violas"
"17 Pizz Violins"
"18 Pizzicato 1"
"19 Pizzicato 2"
"20 Trem String"
"21 Tremulus"
"22 Strgs/Flutes"
"23 Resting Pad"
"24 Divertimento"
"25 Flute"
"26 Piccolo"
    "27 Oboe"
"28 English Horn"
"29 Clarinet"
"30 BassClarinet"
"31 Bassoon"
"32 Contrbasoon"
"33 ChambrWind"
"34 SectionWinds"
"35 Epilogue"
"36 Wind Stack"
"37 FrenchHorn1"
"38 FrenchHorn2"
"39 SectionHorns"
"40 Trumpet 1"
"41 Trumpet 2"
"42 TwoTrumpts"
"43 Harmon Mute"
"44 Trombone 1"
"45 Trombone 2"
"46 Tuba"
"47 Back Brass"
"48 Bright Brass"
"49 ChambrBras"
"50 BrassStrings"
"51 Timpani"
"52 GongCymbl"
"53 Bass/Snare+"
"54 Temple Block"
"55 Xylophone"
"56 Glocknspiel"
"57 Celesta"
"58 TubularBells"
"59 Percussion 1"
"60 Percussion 2"
"61 Harp"
"62 Harpstrings"
"63 Harpsikord"
"64 Notre Dame"
"65 Winter Signs"
"66 Deep Pad"
"67 Portamento"
"68 BellEnsemble"
"69 Cyberspace"
"70 PizzMoogBas"
"71 Marimbala"
"72 Grim Reaper"
"73 Tinker Bell"
"74 Carousel"
"75 Exotic Harp"
"76 Dam Saucers"
"77 Bronze Pad"
"78 Vibraphone"
"79 Astral Flute"
"80 Kool Bass"
"81 Sombre Wind"
"82 SpasCowboy"
"83 The Machine"
"84 Early Perc"
"85 Gently Now"
"86 Piccolodeeyo"
    "87 Infinite One"
"88 Shimmer Way"
"89 Turbo Bass"
"90 Requiem"
"91 Wrong Room"
"92 Analog Pad"
"93 Chapel Organ"
"94 Electrovocal"
"95 Fat Boy Tuba"
"96 SawBassLd"
"97 ViennaDream"
"98 VertigoPad"
"99 Tarkus Twin"
"100 RoomOfStrng"
"101 Magic Bells"
"102 Reginatron"
"103 Sub It!"
"104 Psychlotron"
"105 CloudChambr"
"106 Sepulcher"
"107 Lurch Pluck"
"108 Pizz/Piccolo"
"109 Vampiracal"
"110 String Thing"
"111 Galapagos"
"112 Square One"
"113 Square Link"
"114 <*>"
"115 Sardonicus"
"116 Master Tron"
"117 Lo Wind Inst"
"118 Sympathetic"
    "119 Windchimes"
"120 Boat Haus"
"121 Glitter God"
"122 Story Bass"
"123 Nice Night"
"124 Prophet Lead"
"125 Prophet Link"
"126 Whistl'n Joe"
"127 Link2Shimr"
"128 Ascending"
    ))

(defvar *program-list-roland-1010-orchII*
    '(
      "001 StStrOrc p/f"
      "002 StrSwell/Mod"
      "003 Emotionale"
      "004 Dynamic Str"
      "005 Str DownBow"
      "006 Adagio Str"
      "007 Dyna Marc"
      "008 FourSeasons2"
      "009 StrOrch /Vel"
      "010 StrOrch p/f"
      "011 StatelyBrite"
      "012 RichSymphony"
      "013 St.StrOrch p"
      "014 St.StrOrch f"
      "015 RichStrings2"
      "016 FullStrings2"
      "017 MedStr Sect"
      "018 DynamicRosin"
      "019 101 Violins"
      "020 Natural Str"
      "021 Fast Strings"
      "022 Fat Marcato"
      "023 Strtk 2"
      "024 DecayStrings"
      "025 Piano Str"
      "026 PyanniString"
      "027 Pno/Hrp Str"
      "028 Quartet"
      "029 Solo&Acomp"
      "030 Vln&Vcl"
      "031 Chamber Str"
      "032 St.Cellos"
      "033 String Duet"
      "034 Vln&Vla Atk"
      "035 Stratovolo"
      "036 Vienna Solo"
      "037 Alternate Bow"
      "038 TronStrings2"
      "039 Tape Orchest"
      "040 Reverse Str"
      "041 SoloViolin 1"
      "042 SoloViolin 2"
      "043 Fiddle Spicc"
      "044 FdlGliss Vsw"
      "045 BariolageVln"
      "046 SadSolo Vln"
      "047 Chorus Vln 2"
      "048 Stereo Vln 2"
      "049 Agitato Vln2"
      "050 Solo Cello "
      "051 Dry Cello"
      "052 Solo Vc 3"
      "053 Chorus Cello2"
      "054 Strings&Horns"
      "055 FullOrchest2"
      "056 FilmOrchst p"
      "057 FilmOrchst m"
      "058 St.Orch Uni"
      "059 Horns/ String"
      "060 Woody Orch"
      "061 Orch Ens"
      "062 Orch Unif"
      "063 Orch Brass 1"
      "064 Orch Brass2"
      "065 GRAND Orch"
      "066 Last Nboel"
      "067 Celtic Ensbl"
      "068 Harp & Flute"
      "069 Celesta&Flt"
      "070 Prokofiev"
      "071 Mass In Cee"
      "072 St.OrchBrass"
      "073 Dyn HornSect"
      "074 FrenchyHornz"
      "075 Simple Hornz"
      "076 Large Horns"
      "077 Fr.Horn p>f"
      "078 Fr.Horn sfz2"
      "079 2xFr.Horns"
      "080 Uni Hrns Duo"
      "081 Swell-brass"
      "082 Solo Fr.Hrn1"
      "083 Solo Fr.Hrn2"
      "084 Medieval OYA"
      "085 Orch Horns"
      "086 Brass Ens 9"
      "087 BrassAttack2"
      "088 Full Brass 1"
      "089 Full Brass2"
      "090 Horn Accomp"
      "091 Mood Brass "
      "092 Trumpet Ens"
      "093 3 Trumpets2"
      "094 Centurions"
      "095 Real Trumpet"
      "096 Jazz Trumpet"
      "097 Trumpet 3"
      "098 Trumpet 5"
      "099 El bono"
      "100 Solo Tb 5"
      "101 Solo Tb 6"
      "102 Solo Tb 7 "
      "103 Trombone atm"
      "104 ConcertFlute"
      "105 Flute 2"
      "106 Flute 3"
      "107 Flute 4"
      "108 AmbientFlute"
      "109 Jig Flute"
      "110 Piccolo 2"
      "111 Piccolo 3"
      "112 PiccoloFlt 2"
      "113 Dyno Celt"
      "114 CelticFlt 1"
      "115 CelticFlt 2"
      "116 Celt Fl Trio"
      "117 TinWhistle 1"
      "118 TinWhistle 2"
      "119 TinWhistle 3"
      "120 Celtic Whisl"
      "121 Sinking Duo"
      "122 Beret"
      "123 Hill&Sheeps1"
      "124 Hill&Sheeps2"
      "125 HilandChurch"
      "126 C PentaPipe"
      "127 UileanPipe"
      "128 Celt/Uillean"
      ))

#| 
(defvar *program-list-roland-1010-orchIIB*
    '(
      "129 Celt/Fiddle"
      "130 Oboe Vsw"
      "131 Oboe 6"
      "132 Oboe 7"
      "133 Clarinet 3"
      "134 English Hrn4"
      "135 English Hrn5"
      "136 Bassoon 3"
      "137 Bassoon 4"
      "138 Bassoon 5"
      "139 Oboe / E.Horn"
      "140 DelicateWood"
      "141 Oboe/Clari"
      "142 Jig Duo"
      "143 St.Bousouki"
      "144 Bousouki 1"
      "145 Bouski/Strum"
      "146 Bousouki 2"
      "147 Mafioso"
      "148 String'O Fun"
      "149 BosoukiDream"
      "150 St.Harp"
      "151 Harp Vsw"
      "152 Clear Harp"
      "153 African Harp"
      "154 Rich Harp"
      "155 Harp 3"
      "156 Harp PF"
      "157 MelancolyHrp"
      "158 CelticHarp 1"
      "159 CelticHarp 2"
      "160 Clarsah Harp"
      "161 ElectraHarp"
      "162 ArpeggiHarp"
      "163 Harp Gliss"
      "164 St.Cymbalon"
      "165 Cymbalon"
      "166 Cymbalon Duo"
      "167 Bohemian"
      "168 Child's Toy"
      "169 Xman Piano"
      "170 OnReflection"
      "171 Harmonium 1"
      "172 Harmonium 2"
      "173 St.Music Box"
      "174 Old Mechanic"
      "175 Secret Garden"
      "176 Music Box"
      "177 Victoriana"
      "178 Meditation"
      "179 TubulaBells3"
      "180 TubulaBells4"
      "181 TubulaBells5"
      "182 ChurchBells4"
      "183 BelfryChime2"
      "184 ChurchBells5"
      "185 HappyXmas!"
      "186 Sweet Dreams"
      "187 Huge Bells"
      "188 Glocken 3"
      "189 Mmms & Aaahs"
      "190 Pure Voices"
      "191 CelticSpirit"
      "192 Fem Ahs"
      "193 Perc DigiVox"
      "194 Fates"
      "195 Aah:VelScoop"
      "196 Heaven'sVox1"
      "197 Heaven'sVox2"
      "198 Mmh vox 1"
      "199 Mmms & Aaahs"
      "200 Mmh Vox 2"
      "201 StChr Mm/ Ah"
      "202 Umms & Aahs"
      "203 Humming Chrs"
      "204 Ether Choir"
      "205 SndtrkVoices"
      "206 WinterChoir2"
      "207 BreathVoices"
      "208 Fair...like!"
      "209 She Breathes"
      "210 Movin'Vowels"
      "211 Onya Vox"
      "212 Vox Ghost"
      "213 Spectral Mmh"
      "214 Soft Mover"
      "215 Analogue?"
      "216 Dark Knight"
      "217 Xreeds"
      "218 C.Harp/Pad"
      "219 Phaze Strings"
      "220 Phaze Choirs"
      "221 Dreamsequenz"
      "222 Waterworld"
      "223 Midnight"
      "224 Colombus"
      "225 Titan"
      "226 Interspace"
      "227 fountains"
      "228 SteppedGliss"
      "229 Your Mission"
      "230 Mondo Hit"
      "231 Orch Hit 1"
      "232 big Hit"
      "233 Orch Hit 2"
      "234 Orch FX"
      "235 HorrorHit C4"
      "236 Dyna Timps"
      "237 Timpani 5"
      "238 Dark Drums"
      "239 St.Bodhran"
      "240 Bodhran Vel"
      "241 CrazyBodhran"
      "242 Bodhran Vel"
      "243 Percussn Mix"
      "244 Wind Chimes3"
      "245 Jingle Bell"
      "246 Finger Cym"
      "247 Cym Scrape"
      "248 Tuned Block 1"
      "249 Tuned Block 2"
      "250 Tuned block 3"
      "251 Grongkas"
      "252 Orch Snare"
      "253 Snr Roll Vsw"
      "254 OrchSnr Menu"
      "255 Orch BassDrm"
      "256 OrchDrm Menu"      
      ))
|#

(defvar *program-list-roland-1010-orchIIB*
    '("1 Celt/Fiddle" "2 Oboe Vsw" "3 Oboe 6" "4 Oboe 7" "5 Clarinet 3" "6 English Hrn4"
      "7 English Hrn5" "8 Bassoon 3" "9 Bassoon 4" "10 Bassoon 5" "11 Oboe / E.Horn" "12 DelicateWood"
      "13 Oboe/Clari" "14 Jig Duo" "15 St.Bousouki" "16 Bousouki 1" "17 Bouski/Strum" "18 Bousouki 2"
      "19 Mafioso" "20 String'O Fun" "21 BosoukiDream" "22 St.Harp" "23 Harp Vsw" "24 Clear Harp"
      "25 African Harp" "26 Rich Harp" "27 Harp 3" "28 Harp PF" "29 MelancolyHrp" "30 CelticHarp 1"
      "31 CelticHarp 2" "32 Clarsah Harp" "33 ElectraHarp" "34 ArpeggiHarp" "35 Harp Gliss"
      "36 St.Cymbalon" "37 Cymbalon" "38 Cymbalon Duo" "39 Bohemian" "40 Child's Toy" "41 Xman Piano"
      "42 OnReflection" "43 Harmonium 1" "44 Harmonium 2" "45 St.Music Box" "46 Old Mechanic"
      "47 Secret Garden" "48 Music Box" "49 Victoriana" "50 Meditation" "51 TubulaBells3"
      "52 TubulaBells4" "53 TubulaBells5" "54 ChurchBells4" "55 BelfryChime2" "56 ChurchBells5"
      "57 HappyXmas!" "58 Sweet Dreams" "59 Huge Bells" "60 Glocken 3" "61 Mmms & Aaahs"
      "62 Pure Voices" "63 CelticSpirit" "64 Fem Ahs" "65 Perc DigiVox" "66 Fates" "67 Aah:VelScoop"
      "68 Heaven'sVox1" "69 Heaven'sVox2" "70 Mmh vox 1" "71 Mmms & Aaahs" "72 Mmh Vox 2"
      "73 StChr Mm/ Ah" "74 Umms & Aahs" "75 Humming Chrs" "76 Ether Choir" "77 SndtrkVoices"
      "78 WinterChoir2" "79 BreathVoices" "80 Fair...like!" "81 She Breathes" "82 Movin'Vowels"
      "83 Onya Vox" "84 Vox Ghost" "85 Spectral Mmh" "86 Soft Mover" "87 Analogue?" "88 Dark Knight"
      "89 Xreeds" "90 C.Harp/Pad" "91 Phaze Strings" "92 Phaze Choirs" "93 Dreamsequenz"
      "94 Waterworld" "95 Midnight" "96 Colombus" "97 Titan" "98 Interspace" "99 fountains"
      "100 SteppedGliss" "101 Your Mission" "102 Mondo Hit" "103 Orch Hit 1" "104 big Hit"
      "105 Orch Hit 2" "106 Orch FX" "107 HorrorHit C4" "108 Dyna Timps" "109 Timpani 5"
      "110 Dark Drums" "111 St.Bodhran" "112 Bodhran Vel" "113 CrazyBodhran" "114 Bodhran Vel"
      "115 Percussn Mix" "116 Wind Chimes3" "117 Jingle Bell" "118 Finger Cym" "119 Cym Scrape"
      "120 Tuned Block 1" "121 Tuned Block 2" "122 Tuned block 3" "123 Grongkas" "124 Orch Snare"
      "125 Snr Roll Vsw" "126 OrchSnr Menu" "127 Orch BassDrm" "128 OrchDrm Menu"
      ))





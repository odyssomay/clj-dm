;; Copyright (c) 2004 Lars Frydén, Johan Sundberg, Anders Friberg, Roberto Bresin
;; For information on usage and redistribution, and for a DISCLAIMER OF ALL
;; WARRANTIES, see the files, "README.txt" and "DIRECTORMUSICES.LICENSE.txt", in this distribution.

;; 2004-05-05//rb added copyrightNotice-event class
;; 2005-05-10//af added pitchBend object
;;;061003/af added bank select
;;;061018/af added reverb and pan


;;
;; *****************************************
;;   Definitions for the music data object
;; *****************************************

(in-package :dm)

;;; ====================================================================
;;;     SCORE OBJECT
;;; ==================
;;;

(defclass score ()
  ((score-filename :accessor score-filename :initarg :score-filename :initform NIL)
   (nickname :accessor nickname :initarg :nickname :initform NIL)
   (copyrightnotice-string :accessor copyrightnotice-string :initarg :copyrightnotice-string :initform NIL)
   (sysex-list :accessor sysex-list :initarg :sysex-list :initform '())
   (track-list :accessor track-list :initarg :track-list :initform '())
   ))

(defun score (&rest attributes)
  "Return a score object with specified attributes (keyword value pairs)"
  (apply #'make-instance 'score attributes) )

#|
;for transparent saving/loading to file
(defmethod print-to-file ((self score) stream)
  (format stream "(score~& :track-list~& (list ")
  (dolist (track (track-list self))
    (format stream "~S" track) )
  (format stream "))") )
|#
 
;for printing to file in dm .mus and .per format
(defmethod print-to-file ((self score) stream)
  ;(format stream "(score~& :track-list~& (list ")
  (dolist (track (track-list self))
    (print-to-file track stream) )
   )

(defmethod print-object ((self score) stream)
  (format stream "<SCORE object, nickname: ~S, # of tracks: ~S>"
     (nickname self) (length (track-list self)))
   )

;returns a list of all active tracks
(defmethod active-track-list ((self score))
  (remove-if #'(lambda (track) (not (active-p track))) (track-list self) ))

;returns the total number of segments
(defmethod number-of-segments ((self score))
  (let ((nr 0))
   (dolist (track (track-list self))
     (incf nr (number-of-segments track) ))
    nr ))


(defmethod add-one-track ((self score) track)
   (setf (track-list self) (nconc (track-list self) (list track) )))

(defmethod remove-one-track ((self score) index)
   (setf (track-list self) 
         (delete-if #'(lambda (a) a) (track-list self) :start index :end (1+ index)) )
   )

;;; =====================================================================
;;;     BASIC TRACK OBJECT
;;; =====================================================================
;;;
;;; used to define the basic properties of the tracks
;;; it should be used only for tracks in a meta-track
;;;
(defclass basic-track ()
  ((segment-list :accessor segment-list :initarg :segment-list :initform '())
   ))

; returns the total number of segments in the track
(defmethod number-of-segments ((self basic-track))
  (length (segment-list self)) )

;------ destructive segment-list operations --------------

; add a segment to the end of the segment-list
(defmethod add-one-segment ((self basic-track) segment)
   (setf (segment-list self) (nconc (segment-list self) (list segment) )))

(defmethod remove-one-segment ((self basic-track) index)
   (setf (segment-list self) 
         (delete-if #'(lambda (a) a) (segment-list self) :start index :end (1+ index)) )
   )

(defmethod add-one-segment-before-index ((self basic-track) index segment)
   (setf (segment-list self) 
         (nconc (subseq (segment-list self) 0 index)
           (list segment) 
           (subseq (segment-list self) index)) )
         )


;------var-list accessors for all segments in a track ---------

#|
;set-var sets the value in all segments
(defmethod set-var ((self basic-track) var value)
  (dolist (segment (segment-list self))
     (set-var segment var value)
     ))
;specialized version for voice
;for all vars except sl set only the first segment to the value
(defmethod set-var ((self basic-track) var value)
   (case var
     (sl                        ;sound level set on every segment
       (dolist (segment (segment-list self))
          (set-var segment var value) ))
     ((dr ndr)                  ;duration in proportion to previous values
       (dolist (segment (segment-list self))
          (set-var segment var value) ))
     (t                         ;all the rest: put in the first segment only
       (set-var (car (segment-list self)) var value))
     ))
|#

;specialized for voice tracks
(defmethod set-var ((self basic-track) var value)
   (case var
     (sl                        ;sound level set on every segment
       (dolist (segment (segment-list self))
          (set-var segment var value) ))
     ((dr ndr)                  ;dr and ndr proportionally disregarding locked-dr
      (let ((voweldr 0.0)(consdr 0.0))
         (dolist (segment (segment-list self))
            (if (not (get-var segment 'locked-dr)) (incf voweldr (get-var segment var)))
            (if (get-var segment 'locked-dr) (incf consdr (get-var segment var))) )
         ;(print-ll "voweldr" voweldr "consdr" consdr)
         (let ((factor (/ (- value consdr) voweldr)))
            (if (minusp factor) (warn "too short duration specified, consonants can not fit" (this 'n))
               (dolist (segment (segment-list self))
                  (if (not (get-var segment 'locked-dr)) 
                     (set-var segment var (* (get-var segment var) factor))) )))))
     (t                         ;all the rest: put in the first segment only
       (set-var (car (segment-list self)) var value))
       ))

#|
;returns a list of all values
(defmethod get-var ((self basic-track) var)
   (let ((list '()))
      (dolist (segment (segment-list self))
         (push (get-var segment var) list) )
      (nreverse list) ))

;specialized version for voice
(defmethod get-var ((self basic-track) var)
   (case var
     ;(sl               ;sl returns list
     ;  (let ((list '()))
     ;     (dolist (segment (segment-list self))
     ;        (push (get-var segment var) list) )
     ;     (nreverse list) ))
     ((dr ndr)               ;dr and ndr return the sum of durations
      (let ((drtot 0.0))
         (dolist (segment (segment-list self))
            (incf drtot (get-var segment var)) )
         drtot) )
      (t (get-var (car (segment-list self)) var)) ;the rest returns the value of the first segment
     ))
;;new version returns list if not 'dr or 'ndr
(defmethod get-var ((self basic-track) var)
   (case var
     ((dr ndr)               ;dr and ndr return the sum of durations
      (let ((drtot 0.0))
         (dolist (segment (segment-list self))
            (incf drtot (get-var segment var)) )
         drtot) )
      (t (let ((list '()))  ;otherwise return a list of values
          (dolist (segment (segment-list self))
             (push (get-var segment var) list) )
           (nreverse list) ))
     ))
|#


;;new version returns list if not 'dr or 'ndr
;;removes nil in return list
;;if one element in list --> return element
(defmethod get-var ((self basic-track) var)
   (case var
     ((dr ndr)               ;dr and ndr return the sum of durations
      (let ((drtot 0.0))
         (dolist (segment (segment-list self))
            (incf drtot (get-var segment var)) )
         drtot) )
      (t (let ((list '()))  ;otherwise return a list of values
           (dolist (segment (segment-list self))
             (if (get-var segment var)
             (push (get-var segment var) list) ))
           (nreverse list)
           (if (= (length list) 1) (car list) list)
           ))))

#|
;;testing the use of extra key words for specifying the return values
;;when a track is accessed
(defmethod get-var ((self basic-track) var &key return)
  (cond (return
          (case return
            (:list
             (let ((list '()))
               (dolist (segment (segment-list self))
                 (push (get-var segment var) list) )
               (nreverse list) ))
            ))))

   (case var
     ;(sl               ;sl returns list
     ;  (let ((list '()))
     ;     (dolist (segment (segment-list self))
     ;        (push (get-var segment var) list) )
     ;     (nreverse list) ))
     ((dr ndr)               ;dr and ndr return the sum of durations
      (let ((drtot 0.0))
         (dolist (segment (segment-list self))
            (incf drtot (get-var segment var)) )
         drtot) )
      (t (get-var (car (segment-list self)) var)) ;the rest returns the value of the first segment
     ))
|#

;rem-var removes the var in all segments
(defmethod rem-var ((self basic-track) var)
  (dolist (segment (segment-list self))
     (rem-var segment var)
     ))

;get the total duration 
(defmethod get-dr ((self basic-track))
 (let ((drtot 0.0))
  (dolist (segment (segment-list self))
    (incf drtot (get-dr segment)) )
  drtot))

;multiply all durations with a factor
(defmethod multiply-dr ((self basic-track) factor)
 (dolist (segment (segment-list self))
   (multiply-dr segment factor) ))

;set the total duration to dr (ms)
;all segments will be proportionally changed
(defmethod set-dr ((self basic-track) dr)
 (multiply-dr self (/ dr (get-dr self))) )

#|
;returns the segment occuring at time drabs relative the beginning of track
(defmethod get-segment-at-drabs ((self basic-track) drabs)
   (let ((drsum 0))
      (dolist (segment (segment-list self))
         (incf drsum (get-dr segment))
         (if (< drabs drsum) (return segment))
        )))
|#
;;returns both segment and index
(defmethod get-segment-at-drabs ((self basic-track) drabs)
   (let ((drsum 0) (index -1))
     (dolist (segment (segment-list self))
       (incf index)
       (incf drsum (get-dr segment))
       (if (< drabs drsum) (return (values segment index)))
        )))

#|
;returns the segment occuring at time ndrabs relative the beginning of track
(defmethod get-segment-at-ndrabs ((self basic-track) ndrabs)
   (let ((ndrsum 0))
      (dolist (segment (segment-list self))
         (incf ndrsum (get-var segment 'ndr))
         (if (< ndrabs ndrsum) (return segment))
        )))
|#

(defmethod get-segment-at-ndrabs ((self basic-track) ndrabs)
   (let ((ndrsum 0)(index -1))
     (dolist (segment (segment-list self))
       (incf index)
       (incf ndrsum (get-var segment 'ndr))
       (if (< ndrabs ndrsum) (return (values segment index)))
         )))

;------------ track ----------------------------------------------

;the basic track used for "real" tracks
;it is not used directly
;;; (defclass track (basic-track)
;;;   ((active-p :accessor active-p :initarg :active-p :initform t)
;;;    (trackname :accessor trackname :initarg :trackname :initform "track")
;;;    (midi-channel :accessor midi-channel :initarg :midi-channel :initform 1)
;;;    (midi-initial-volume :accessor midi-initial-volume
;;;      :initarg :midi-initial-volume :initform 0)
;;;    (midi-initial-program :accessor midi-initial-program
;;;      :initarg :midi-initial-program :initform 1)
;;;    (synth :accessor synth :initarg :synth :initform nil)
;;;    (instrument-type :accessor instrument-type :initarg :instrument-type :initform "String")
;;;    (segment-list :accessor segment-list :initarg :segment-list :initform '())
;;;    ;(meta-track-list :accessor meta-track-list :initarg :meta-track-list :initform '())
;;;    ))
;;;(defclass track (basic-track)
;;;  ((active-p :accessor active-p :initarg :active-p :initform t)
;;;   (trackname :accessor trackname :initarg :trackname :initform "track")
;;;   (midi-channel :accessor midi-channel :initarg :midi-channel :initform 1)
;;;   (midi-initial-volume :accessor midi-initial-volume
;;;     :initarg :midi-initial-volume :initform 0)
;;;   (midi-initial-program :accessor midi-initial-program
;;;     :initarg :midi-initial-program :initform 1)
;;;   (synth :accessor synth :initarg :synth :initform nil)
;;;   (instrument-type :accessor instrument-type :initarg :instrument-type :initform "String")
;;;   (track-delay :accessor track-delay :initarg :track-delay :initform 0)
;;;   (segment-list :accessor segment-list :initarg :segment-list :initform '())
;;;   ;(meta-track-list :accessor meta-track-list :initarg :meta-track-list :initform '())
;;;   ))

(defclass track (basic-track)
  ((active-p :accessor active-p :initarg :active-p :initform t)
   (trackname :accessor trackname :initarg :trackname :initform "track")
   (midi-channel :accessor midi-channel :initarg :midi-channel :initform 1)
   (midi-initial-volume :accessor midi-initial-volume :initarg :midi-initial-volume :initform 0)
   (midi-initial-program :accessor midi-initial-program :initarg :midi-initial-program :initform 1)
   (midi-bank-msb :accessor midi-bank-msb :initarg :midi-bank-msb :initform nil)
   (midi-bank-lsb :accessor midi-bank-lsb :initarg :midi-bank-lsb :initform nil)
   (midi-pan :accessor midi-pan :initarg :midi-pan :initform nil)
   (midi-reverb :accessor midi-reverb :initarg :midi-reverb :initform nil)
   (synth :accessor synth :initarg :synth :initform (make-synth (get-dm-var 'play-synth-name-default)))
   (instrument-type :accessor instrument-type :initarg :instrument-type :initform "String")
   (track-delay :accessor track-delay :initarg :track-delay :initform 0)
   (segment-list :accessor segment-list :initarg :segment-list :initform '())
   ;(meta-track-list :accessor meta-track-list :initarg :meta-track-list :initform '())
   ))

;-------------------multi track---------------------------------------------------
; a track which is not restricted to sequential segments
;rather works like a midifile track
;needs specialized methods for all accesses
;and abstime
(defclass multi-track (track)())


;-------------------mono track-------------------------------------------------
;a strict list of segments
;the duration of each segment is the same as the interonset duration to next
;segment
(defclass mono-track (track)())

(defun mono-track (&rest attributes)
  "Return a mono-track object with specified attributes (keyword value pairs)"
  (apply #'make-instance 'mono-track attributes) )

#|
;for transparent saving/loading to file
(defmethod print-to-file ((self mono-track) stream)
  (format stream 
    "(mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :synth ~S~& :segment-list~& (list "
   "mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :synth ~S~&"
          (trackname self) (midi-channel self) (midi-initial-volume self)(synth self))
  (dolist (segment (segment-list self))
     (terpri stream)
    (print-to-file segment stream) )
  (format stream "))") )
|#

;for printing to file in dm .mus and .per format
#|
(defmethod print-to-file ((self mono-track) stream)
  (let ((*print-right-margin* 10000)) (format stream 
   "mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :track-delay ~S~& :midi-initial-program ~S~& :synth ~S~&"
    (trackname self) (midi-channel self)
    (midi-initial-volume self)(track-delay self)(midi-initial-program self)
    (synth-symbol-to-name (type-of (synth self))) )   ;synth name from type
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
    ))
|#
;;added midi bank
(defmethod print-to-file ((self mono-track) stream)
  (let ((*print-right-margin* 10000)) (format stream 
    "mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :track-delay ~S~& :midi-bank-msb ~S~& :midi-bank-lsb ~S~& :midi-initial-program ~S~& :midi-pan ~S~& :midi-reverb ~S~& :synth ~S~&"
    (trackname self) (midi-channel self)
    (midi-initial-volume self)(track-delay self)(midi-bank-msb self)(midi-bank-lsb self)(midi-initial-program self)(midi-pan self)(midi-reverb self)
    (synth-symbol-to-name (type-of (synth self))) )   ;synth name from type
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
   ))

#|
(defmethod print-object ((self mono-track) stream)
  (format stream "<MONO-TRACK object, trackname: ~S, midi-channel ~S, # of segments: ~S>"
     (trackname self) (midi-channel self)(length (segment-list self)))
  )
|#

(defmethod print-object ((self mono-track) stream)
  (format stream "<mono-track ~S>" (trackname self)) )
 

;(defmethod print-object ((self mono-track) stream)  (format stream (trackname self)) )


;------------------meta track---------------------------------------------------

;a track with a list of sub-tracks
(defclass meta-track (basic-track)
 ( (trackname :accessor trackname :initarg :trackname :initform "")
   (track-list :accessor track-list :initarg :track-list :initform '())
   ))

(defmethod add-one-track ((self meta-track) track)
   (setf (track-list self) (nconc (track-list self) (list track) )))


 
;--------------------voice track-----------------------------------------------

(defclass voice-track (track)())

(defun voice-track (&rest attributes)
  "Return a voice-track object with specified attributes (keyword value pairs)"
  (apply #'make-instance 'voice-track attributes) )

#|
;for transparent saving/loading to file
(defmethod print-to-file ((self voice-track) stream)
  (format stream 
   "(voice-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :synth ~S~& :segment-list~& (list "
          (trackname self) (midi-channel self) (midi-initial-volume self)(synth self))
  (dolist (segment (segment-list self))
    (format stream "~&   ~S" segment) )
  (format stream "))") )
 
;for printing to file in dm .mus and .per format
(defmethod print-to-file ((self voice-track) stream)
  (format stream 
   "voice-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :midi-initial-program ~S~& :synth ~S~&"
          (trackname self) (midi-channel self) (midi-initial-volume self)(midi-initial-program self)(synth self))
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
  )
|#

#|
(defmethod print-to-file ((self voice-track) stream)
  (let ((*print-right-margin* 10000)) (format stream 
   "voice-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :track-delay ~S~& :midi-initial-program ~S~& :synth ~S~&"
    (trackname self) (midi-channel self)
    (midi-initial-volume self)(track-delay self)(midi-initial-program self)
    (synth-symbol-to-name (type-of (synth self))))   ;synth name from type
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
    ))
|#
(defmethod print-to-file ((self voice-track) stream)
  (let ((*print-right-margin* 10000)) (format stream 
    "mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :track-delay ~S~& :midi-bank-msb ~S~& :midi-bank-lsb ~S~& :midi-initial-program ~S~& :midi-pan ~S~& :midi-reverb ~S~& :synth ~S~&"
    (trackname self) (midi-channel self)
    (midi-initial-volume self)(track-delay self)(midi-bank-msb self)(midi-bank-lsb self)(midi-initial-program self)(midi-pan self)(midi-reverb self)
    (synth-symbol-to-name (type-of (synth self))) )   ;synth name from type
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
   ))

(defmethod print-object ((self voice-track) stream)
  (format stream "<VOICE-TRACK object, trackname: ~S, midi-channel ~S, # of segments: ~S>"
     (trackname self) (midi-channel self)(length (segment-list self)))
   )



;;; ====================================
;;; segment object
;;; ====================================
;;;
(defclass segment ()
  ((var-list :accessor var-list :initarg :var-list :initform '())
   ))

(defun segment (&rest attributes)
  "Return a segment object with specified attributes (keyword value pairs)"
  (apply #'make-instance 'segment attributes) )

#|
;for transparent saving/loading to file
(defmethod print-to-file ((self segment) stream)
  (format stream "(segment :var-list '~S)" (alist-to-list (var-list self))) )
|#

;for printing to file in dm .mus and .per format 
;hopefully no newline
(defmethod print-to-file ((self segment) stream)
  (let ((*print-right-margin* 10000)) (format stream "~S~&" (alist-to-list (var-list self)))) )

(defmethod print-object ((self segment) stream)
  (format stream "<SEGMENT object, ndr: ~D, n or ph: ~S>" 
    (round (get-var self 'ndr))(or (get-var self 'n) (get-var self 'ph))) )

;---- get a modified var-list for clj-dm ------
;a new prop :note-value-fraction is added for the note value converted to a fraction
;works for both old and new notation including dot, tuple etc,
;120416/af

(defmethod var-list-clj-dm ((self segment))
  (acons :note-value-fraction (get-note-value-fraction-from-segment self)
         (var-list self) ))

;------segment var-list accessors ---------

(defmethod set-var ((self segment) var value)
  (if (assoc var (var-list self))
    (rplacd (assoc var (var-list self)) value)
    (setf (var-list self) (acons var value (var-list self)))
    ))

(defmethod get-var ((self segment) var)
  (cdr (assoc var (var-list self))) )

(defmethod rem-var ((self segment) var)
  (setf (var-list self) (delete (assoc var (var-list self)) (var-list self)) ))


(defmethod get-dr ((self segment))
  (get-var self 'dr) )

(defmethod set-dr ((self segment) dr)
  (set-var self 'dr dr) )

(defmethod multiply-dr ((self segment) factor)
  (set-dr self (* (get-dr self) factor)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  class midifile
;;

(defclass midifile (score)
  ((miditype :accessor miditype :initarg :miditype :initform NIL  ;; type of MIDI file: 0,1 or 2 (format in SMF spec 1.0)
         :type integer)
   (ntrks :accessor ntrks :initarg ntrks :initform NIL         ;; number of tracks
          :type integer)
   (division :accessor division :initarg :division :initform NIL
             :type integer)
   ))

(defmethod print-object ((self midifile) stream)
  (format stream "<MIDIFILE object, nickname: ~A miditype: ~A ntrks: ~A division: ~A>" 
          (nickname self)(miditype self) (ntrks self)(division self)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  --- midi track ---
;;

(defclass midi-track (track)())

#|
(defmethod print-to-file ((self midi-track) stream)
  (format stream 
   "(midi-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :synth ~S~& :segment-list~& (list "
          (trackname self) (midi-channel self) (midi-initial-volume self)(synth self))
  ; (format stream "~&   ~S" (segment-list self))
  ;(dolist (segment (segment-list self))
  ;  (format stream "~&   ~S" segment) )
  (format stream "))") )

;for printing to file in dm .mus and .per format
(defmethod print-to-file ((self midi-track) stream)
  (format stream 
   "mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :synth ~S~&"
          (trackname self) (midi-channel self) (midi-initial-volume self)(synth self))
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
  )
|#
 
;for printing to file in dm .mus and .per format
#|
(defmethod print-to-file ((self midi-track) stream)
  (let ((*print-right-margin* 10000)) (format stream 
   "mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :track-delay ~S~& :midi-initial-program ~S~& :synth ~S~&"
    (trackname self) (midi-channel self)
    (midi-initial-volume self)(track-delay self)(midi-initial-program self)
    (synth-symbol-to-name (type-of (synth self))))   ;synth name from type
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
    ))
|#
(defmethod print-to-file ((self midi-track) stream)
  (let ((*print-right-margin* 10000)) (format stream 
    "mono-track~& :trackname ~S~& :midi-channel ~S~& :midi-initial-volume ~S~& :track-delay ~S~& :midi-bank-msb ~S~& :midi-bank-lsb ~S~& :midi-initial-program ~S~& :midi-pan ~S~& :midi-reverb ~S~& :synth ~S~&"
    (trackname self) (midi-channel self)
    (midi-initial-volume self)(track-delay self)(midi-bank-msb self)(midi-bank-lsb self)(midi-initial-program self)(midi-pan self)(midi-reverb self)
    (synth-symbol-to-name (type-of (synth self))) )   ;synth name from type
  (dolist (segment (segment-list self))
    (print-to-file segment stream) )
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  --- segments used in midi tracks of an object of type midifile ---
;;

(defclass MTrk-event (segment)
  ((delta-time :accessor delta-time
               :initarg :delta-time
               :type integer)))

(defclass MIDI-event (MTrk-event) ())

(defclass sysex-event (MTrk-event) 
  ((sysex :reader sysex
          :initarg :sysex
          :type list)) )

(defclass meta-event (MTrk-event) ())

(defclass setTempo (meta-event)
  ((midi-tempo :reader midi-tempo
          :initarg :midi-tempo
          :type integer)))

(defclass copyrightNotice-event (meta-event)
  ((copyrightNotice :reader copyrightNotice
	            :initarg :copyrightNotice
	            :type string)))

(defmethod print-object ((self settempo) stream)
  (format stream "#<setTempo object, tempo: ~D>" 
          (midi-tempo self)))

(defclass timeSignature (meta-event)
  ((nn :reader nn
       :initarg :nn
       :type integer)
   (dd :reader dd
       :initarg :dd
       :type integer)
   (cc :reader cc
       :initarg :cc
       :type integer)
   (bb :reader bb
       :initarg :bb
       :type integer)))

(defmethod print-object ((self timesignature) stream)
  (format stream "<timeSignature object, nn: ~D dd: ~D cc: ~D bb: ~D>" 
          (nn self)(dd self) (cc self)(bb self)))

(defclass keySignature (meta-event)
  ((sf :reader sf
       :initarg :sf
       :type integer)
   (mi :reader mi
       :initarg :mi
       :type integer)))

(defmethod print-object ((self keysignature) stream)
  (format stream "<KEYSIGNATURE object, sf: ~D mi: ~D>" 
          (sf self)(mi self)))

(defclass endOfTrack (meta-event) ())

(defmethod print-object ((self endOfTrack) stream)
  (format stream "<~D endOftrack>" 
          (delta-time self)))

(defclass noteOn (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (note-number :reader note-number
                :initarg :note-number
                :type integer)
   (key-velocity :reader velocity
                 :initarg :velocity
                 :type integer)))

(defclass noteOff (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (note-number :reader note-number
                :initarg :note-number
                :type integer)
   (key-velocity :reader velocity
                 :initarg :velocity
                 :type integer)))

(defmethod print-object ((self noteOn) stream)
  (format stream "<~D noteOn ~D ~D>" 
          (delta-time self)(note-number self) (velocity self)))

(defmethod print-object ((self noteOff) stream)
  (format stream "<~D noteOff ~D ~D>" 
          (delta-time self)(note-number self) (velocity self)))

(defclass programChange (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (program :reader program
                :initarg :program
                :type integer
                :initform NIL)
   ))

(defmethod print-object ((self programChange) stream)
  (format stream "<~D programChange: ~D>" 
          (delta-time self) (program self)))

(defclass midiVolume (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (volume :reader volume
                :initarg :volume
                :type integer
                :initform NIL)
   ))

(defmethod print-object ((self midiVolume) stream)
  (format stream "<~D midiVolume: ~D>" 
          (delta-time self)(volume self)))

(defclass midiBankMSB (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (msb :reader msb
                :initarg :msb
                :type integer
                :initform NIL)
   ))

(defmethod print-object ((self midiBankMSB) stream)
  (format stream "<~D midiBankMSB: ~D>" 
          (delta-time self)(msb self)))

(defclass midiBankLSB (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (lsb :reader lsb
                :initarg :lsb
                :type integer
                :initform NIL)
   ))

(defmethod print-object ((self midiBankLSB) stream)
  (format stream "<~D midiBankLSB: ~D>" 
          (delta-time self)(lsb self)))

(defclass midiPan (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (pan :reader pan
                :initarg :pan
                :type integer
                :initform NIL)
   ))

(defmethod print-object ((self midiPan) stream)
  (format stream "<~D midiPan: ~D>" 
          (delta-time self)(pan self)))

(defclass midiReverb (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (reverb :reader reverb
                :initarg :reverb
                :type integer
                :initform NIL)
   ))

(defmethod print-object ((self midiReverb) stream)
  (format stream "<~D midiReverb: ~D>" 
          (delta-time self)(reverb self)))

(defclass pitchBend (MIDI-event)
  ((channel :reader channel
            :initarg :channel
            :type integer)
   (pitchbend :reader pitchbend
                :initarg :pitchbend
                :type integer
                :initform NIL)
   ))

(defmethod print-object ((self pitchBend) stream)
  (format stream "<~D pitchBend: ~D>" 
          (delta-time self) (pitchbend self)))

;; -----------------
;;   LIST-TO-ALIST
;; -----------------
;;
(defun list-to-alist (list)
  (let ((alist '()))
    (loop while list do
          (push (cons (pop list) (pop list)) alist ) )
    (nreverse alist) ))

(defun alist-to-list (alist)
  (let ((list '()))
   (loop while alist do
     (let ((cons (pop alist)))
      (push (car cons) list)
      (push (cdr cons) list) ))
   (nreverse list) ))


;;-----------------
;;  TIME-SHAPE OBJECT
;;   associated with segments
;;   no times outside the segment allowed
;;  ***** MOVED TO SHAPEOBJECTS.LSP **********
;;-----------------
#|
(defclass time-shape ()
 ((time-value-list :accessor time-value-list :initarg :time-value-list :initform NIL)
     (interpolation :accessor interpolation :initarg :interpolation :initform :linear)
      (used-segment :accessor used-segment :initarg :used-segment :initform nil)
  ))


(defun time-shape (&rest attributes)
 "Return a time-shape object with specified attributes (keyword value pairs)"
 (apply #'make-instance 'time-shape attributes) )



;;returns an interpolated value for a given time
(defmethod get-value ((self time-shape) time-rel-onset)
  (let ((n 0) time1 time2 val1 val2 (tv-list (time-value-list self))
         (dr (get-var (used-segment self) 'dr)) )
  
     (print-ll tv-list dr)
     ;;select the two nearest times
     (untilexit loopa
       (setq time2 (timereflist-to-time (car (nth n tv-list)) dr))
       (print-ll time1 " " time2)
       (cond ((< time-rel-onset time2)
              (setq time1 (timereflist-to-time (car (nth (1- n) tv-list)) dr))
              (setq val1 (cadr (nth (1- n) tv-list)))
              (setq val2 (cadr (nth n tv-list)))
              (return-from loopa) )
             ((= time-rel-onset time2)
              (setq val2 (cadr (nth n tv-list)))
              (setq val1 val2)
              (return-from loopa) ))
       (incf n) )

     ;;interpolate if not equal values
     (if (= val1 val2)
        val1
        (case (interpolation self)
        (:linear (dr-linear  time-rel-onset time1 (float val1) time2 val2))
          ))))


             
(defun timereflist-to-time (timereflist segment-dur)
  (cond 
     ((numberp timereflist) timereflist)
     ((listp timereflist)
      (case (car timereflist)
       (:start (cadr timereflist))
       (:end (+ segment-dur (cadr timereflist)))
       (:relative (* segment-dur (cdr timereflist)))
       ))
        (t (warn "Not an allowed time reference in time-shape: ~S" (car timereflist)))
         ))


|#

;;eof

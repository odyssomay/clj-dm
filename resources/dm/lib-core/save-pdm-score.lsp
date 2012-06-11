;;;-*-Mode: LISP; Package: DM -*-

;;;A tool for exporting the deviations of each individual rule
;;;to be used in the real-time expressive sequencer pDM in pd
;;; the file extension for this version is "pdm"
;;;2004-06-21/Anders Friberg
;;;
;;;041102/af changed rule list to include 4 phrasing levels
;;;041108/af completely new format using only affected parameters
;;;050331/af added chords, tempo, key, meter info
;;;050401/af added simultaneous notes
;;;061003/af added bank select
;;;061018/af added reverb and pan
;;;070814/af added DSL and DART for each chord note, fixed problem with nsl

;;;;************bör fixa att articulation hämtas från sista ej bundna tonen - annars blir det fel ton*********************


;;;-----------FORMAT----------------

(in-package :dm)


;;main function from the menu
;;opens a file save dialog
(defun pdm-save-rule-data ()
   (let ((fpath (show-dialog-for-saving-files-PD "Save individual rule data (pDM format)"
                 :directory (merge-pathnames (make-pathname :type "pdm") (score-filename *active-score*))
                 :extensions '(("MUS score files" . "*.pdm")("All files" . "*.*")) )))
     (if fpath (with-waiting-cursor 
                 (pdm-save-rule-data-fpath fpath) )
       (print "I could not save the file") )) 
  )

;main function using file path input
(defun pdm-save-rule-data-fpath (fpath)
  (with-open-file (ofile fpath :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
    (make-simple-syncmel)
    (pdm-apply-rules *pdm-rule-list*)
    (pdm-print-rule-data-stream ofile)
    (remove-last-track) ; get rid of the sync track
    (rem-all :artvec)
    (rem-all :slvec)
    )
  (if (get-dm-var 'verbose-i/o)
      (print-ll "Active score and rule deviations saved in pDM format: " fpath))
  )

#|
; ashort version for testing
(setq *pdm-rule-list*
      '(
       (high-loud 1)
       (duration-contrast 1)
       (punctuation 1)
        )) 

|#

;; rule and rule parameters; tempo index; sound level index; articulation index
;; nil means rule doesn't change this parameter
(defvar *pdm-rule-list*)
(setq *pdm-rule-list*
      '(
        ((Phrase-Arch 1 :Phlevel 4 :Turn 0.7 :Next 1.3 :Amp 2) 1   1   nil)
        ((Phrase-Arch 1 :Phlevel 5 :Turn 0.5 :Next 1.3 :Amp 2) 2   2   nil)
        ((Phrase-Arch 1 :Phlevel 6 :Turn 0.5 :Amp 2 :Last 0.2) 3   3   nil)
        ((Phrase-Arch 1 :Phlevel 7 :Turn 0.5 :Amp 2 :Last 0.2) 4   4   nil)
        ((Phrase-ritardando 1 :Phlevel 4)                      5   5   nil)
        ((Phrase-ritardando 1 :Phlevel 5)                      6   6   nil)
        ((Phrase-ritardando 1 :Phlevel 6)                      7   7   nil)
        ((final-ritard 1)                                      8   nil nil)
        ((punctuation 1)                                       9   nil 1)
        ((high-loud 1)                                         nil 8   nil)
        ((melodic-charge 1)                                    10  9   nil)
        ((harmonic-charge 1)                                   11  10  nil)
        ((duration-contrast 1)                                 12  11   nil)
        ((Inegales 1)                                          13  nil nil)
        ((double-duration 1)                                   14  nil nil)
        ((repetition-articulation-dro 1)                       nil nil 2)
        ((score-legato-art 1)                                  nil nil 3)
        ((score-staccato-art 1)                                nil nil 4)
        ((overall-articulation 1)                              nil nil 5)
        ))

;;the total number of parameters for tempo, SL and articulation
(defvar *pdm-tvec-size*)
(defvar *pdm-slvec-size*)
(defvar *pdm-artvec-size*)
(setq *pdm-tvec-size* 14)
(setq *pdm-slvec-size* 11)
(setq *pdm-artvec-size* 5)

; applies a list of rules and computes the deviations from each
; makes a sync track from which all tempo deviations are stored
; fixed problem with nsl
#|
(defun pdm-apply-rules (rulelist)
  (let (vec)
    ;(make-simple-syncmel)
    ;-------initialize deviation vectors for each note-------------
    (each-track
     (if (string-equal (trackname *this-track*) "sync-track")
         (each-note
          (set-this :tvec (make-array *pdm-tvec-size* :element-type 'single-float :initial-element 0.0))
          )
       (each-note-if
        (not (this 'rest))
        (then
         (set-this :slvec (make-array *pdm-slvec-size* :element-type 'single-float :initial-element 0.0))
         (set-this :artvec (make-array *pdm-artvec-size* :element-type 'fixnum :initial-element 0))
         ))))
    ;----apply each rule from the list and collect deviations
    (reset-music)
    ;(print "reset ok")
    (dotimes  (i (length rulelist))
      (let ((ti (second (nth i rulelist)))  ;get index into time dev vector
            (sli (third (nth i rulelist)))
            (arti (fourth (nth i rulelist))) )
        (print (first (nth i rulelist)))
        (eval (first (nth i rulelist))) ;apply the rule
        (if sli (normalize-sl))
        (if ti (normalize-dr))
        ;(print "rule ok")
        (each-track
         (if (and ti (string-equal (trackname *this-track*) "sync-track"))
             (each-note
              (setq vec (this :tvec))
              (setf (aref vec (1- ti)) (round2two (- (/ (this 'ndr) (this 'dr)) 1.0))) ;percent increase in tempo
              (set-this :tvec vec)
              ))
         (if (and sli (not (string-equal (trackname *this-track*) "sync-track")))
             (each-note-if
              (this :slvec)
              (then
               (setq vec (this :slvec))
               (setf (aref vec (1- sli)) (round2one (float (- (this-or-zero 'sl) (this-or-zero 'nsl)))))
               (set-this :slvec vec)
               )))
         (if (and arti (not (string-equal (trackname *this-track*) "sync-track")))
             (each-note-if
              (this :artvec)
              (then
               (setq vec (this :artvec))
               (setf (aref vec (1- arti)) (round (- (this-or-zero 'dro))))
               (set-this :artvec vec)
               )))
         ))
      ;(print "vector ok")
      (reset-music)
      )))
|#

(defun pdm-apply-rules (rulelist)
  (let (vec)
    ;(make-simple-syncmel)
    ;-------initialize deviation vectors for each note-------------
    (each-track
     (if (string-equal (trackname *this-track*) "sync-track")
         (each-note
          (set-this :tvec (make-array *pdm-tvec-size* :element-type 'single-float :initial-element 0.0))
          )
       (each-note-if
        (not (this 'rest))
        (then
         (set-this :slvec (make-array *pdm-slvec-size* :element-type 'single-float :initial-element 0.0))
         (set-this :artvec (make-array *pdm-artvec-size* :element-type 'fixnum :initial-element 0))
         ))))
    ;----apply each rule from the list and collect deviations
    (reset-music)
    ;(print "reset ok")
    (dotimes  (i (length rulelist))
      (let ((ti (second (nth i rulelist)))  ;get index into time dev vector
            (sli (third (nth i rulelist)))
            (arti (fourth (nth i rulelist))) )
        (print (first (nth i rulelist)))
        (eval (first (nth i rulelist))) ;apply the rule
        (if sli (normalize-sl))
        (if ti (normalize-dr))
        ;(print "rule ok")
        (each-track
         (if (and ti (string-equal (trackname *this-track*) "sync-track"))
             (each-note
              (setq vec (this :tvec))
              (setf (aref vec (1- ti)) 
                (if (> (this 'dr) 1.0) 
                    (round2two (- (/ (this 'ndr) (this 'dr)) 1.0)) ;percent increase in tempo
                  0.0 ;fix for very short dr - shouldn't affect anything important
                  ))         
              (set-this :tvec vec)
              ))
         (if (and sli (not (string-equal (trackname *this-track*) "sync-track")))
             (each-note-if
              (this :slvec)
              (then
               (setq vec (this :slvec))
               (setf (aref vec (1- sli)) (round2one (float (- (this-or-zero 'sl) (this-or-zero 'nsl)))))
               (set-this :slvec vec)
               )))
         (if (and arti (not (string-equal (trackname *this-track*) "sync-track")))
             (each-note-if
              (this :artvec)
              (then
               (setq vec (this :artvec))
               (setf (aref vec (1- arti)) (round (- (this-or-zero 'dro))))
               (set-this :artvec vec)
               )))
         ))
      ;(print "vector ok")
      (reset-music)
      )))

#|
;;a test for generating radio baton track
(defun mark-beat-in-sync-track ()
    (each-track
     (if (string-equal (trackname *this-track*) "sync-track")
         (mark-beat)
       (each-note-if
        (this :beat)
        (not (first?))
        (then
         (set-prev :bef-beat t)
       )))))
|#


;produces a list of event with absolute time in the beginning that is then sorted
;this is similar to the playlist file but only with program change in the beginning
;can easily be extended with pitch bend etc.

;;;(defun pdm-make-playlist ()
;;;  (let ((abstime 0)(bigl '())(chan 1) )
;;;    (newr bigl (list abstime "FILETYPE" "pDM_score_file")) ;file type
;;;    (newr bigl (list abstime "VERSION" 1.3)) ;file version
;;;    (newr bigl (list abstime "HEADERSTART" 1)) ;
;;;    (newr bigl (list (+ (round abstime) 0.1) "HEADEREND" 1)) ;
;;;    (newr bigl (list abstime "SCORENAME" (pathname-name (score-filename *active-score*)))) ;score name
;;;    (each-track
;;;     (setq abstime 0)
;;;     ;;(incf abstime (get-track-var 'track-delay)) ;gets modulated by tempo variations in pDM - maybe not intended!
;;;     (if (not (string-equal (trackname *this-track*) "sync-track"))
;;;         (then 
;;;          (if (get-track-var 'midi-channel) (setq chan (get-track-var 'midi-channel))) ;get midi channel
;;;          (newr bigl (list abstime "TRACKNAME" (get-track-var 'trackname) chan)) ;trackname
;;;          (newr bigl (list abstime "SYNTH" (synth-symbol-to-name (type-of (get-track-var 'synth))) chan)) ;synth name
;;;          (newr bigl (list abstime "PROGRAM" (get-track-var 'midi-initial-program) chan)) ;inital program 
;;;          (newr bigl (list abstime "VOL" (get-track-var 'midi-initial-volume) chan)) ;inital volume 
;;;          (newr bigl (list abstime "TRACKDELAY" (get-track-var 'track-delay) chan)) ;track delay
;;;          (each-note ;---all the normal tracks---
;;;           (when (not (this 'rest))
;;;             (newr bigl (list (+ (round abstime) 0.2) "DSL" (this :slvec))) ;DSL
;;;             (newr bigl (list (+ (round abstime) 0.2) "DART" (this :artvec))) ; DART
;;;             (newr bigl (list (+ (round abstime) 0.3) "NOTE" (this-f0) chan (round2one (this-or-zero 'sl)) (round (this 'ndr)))) ;NOTE
;;;             )
;;;           (incf abstime (this 'ndr)) ))
;;;       (each-note ;---sync track tempo stuff---
;;;        (if (this 'bar) (newr bigl (list (+ (round abstime) 0.15) "BAR" (this 'bar))))
;;;        (newr bigl (list (+ (round abstime) 0.15) "DT" (this :tvec)))
;;;        (incf abstime (this 'ndr))
;;;        )))
;;;    (sort bigl 'compare)))

;;added tied notes **********fix also articulation for these*************
;;added chords, tempo, key, meter info
;;added simultaneous notes
;;problem: vid flera samtidiga toner kommer alla art och sl instruktioner först dvs ej till rätt ton
;;- sätt in channel info i dsl och dart
#|
(defun pdm-make-playlist ()
  (let ((abstime 0)(bigl '())(chan 1) )
    (newr bigl (list abstime "FILETYPE" "pDM_score_file")) ;file type
    (newr bigl (list abstime "VERSION" 1.3)) ;file version
    (newr bigl (list abstime "HEADERSTART" 1)) ;
    (newr bigl (list (+ (round abstime) 0.1) "HEADEREND" 1)) ;
    (newr bigl (list abstime "SCORENAME" (pathname-name (score-filename *active-score*)))) ;score name
    (if (get-first 'meter) (newr bigl (list abstime "METER" (car (get-first 'meter)) (cadr (get-first 'meter))))) ;meter
    (if (get-first 'mm) (newr bigl (list abstime "TEMPO" (mm-to-microsec (get-first 'mm))))) ;tempo
    (if (and (get-first 'key)(get-first 'modus)) (newr bigl (list abstime "KEY" (get-first 'key) (get-first 'modus)))) ;key
    (each-track
     (setq abstime 0)
     ;;(incf abstime (get-track-var 'track-delay)) ;gets modulated by tempo variations in pDM - maybe not intended!
     (if (not (string-equal (trackname *this-track*) "sync-track"))
         (then 
          (if (get-track-var 'midi-channel) (setq chan (get-track-var 'midi-channel))) ;get midi channel
          (newr bigl (list abstime "TRACKNAME" (get-track-var 'trackname) chan)) ;trackname
          (newr bigl (list abstime "SYNTH" (synth-symbol-to-name (type-of (get-track-var 'synth))) chan)) ;synth name
          (newr bigl (list abstime "PROGRAM" (get-track-var 'midi-initial-program) chan)) ;inital program 
          (newr bigl (list abstime "VOL" (get-track-var 'midi-initial-volume) chan)) ;inital volume 
          (newr bigl (list abstime "TRACKDELAY" (get-track-var 'track-delay) chan)) ;track delay
          (each-note ;---all the normal tracks---
           (if (this 'q) (newr bigl (list (+ (round abstime) 0.16) "CHORD" (chordname-to-rootname (this 'q)) (chordname-to-modus (this 'q))))) ;chords
           (when (not (this 'rest))
             (newr bigl (list (+ (round abstime) 0.2) "DSL" (this :slvec)))     ; DSL
             (newr bigl (list (+ (round abstime) 0.2) "DART" (this :artvec)))   ; DART
             (if (or (first?) (not (prev 'tie)))                                ; NOTE
                 (if (listp (this 'f0))
                     (dolist (f0 (this 'f0))
                       (newr bigl (list (+ (round abstime) 0.3) "NOTE" f0 chan (round2one (this-or-zero 'sl))
                                        (if (this 'tie) (round (this-sounding-ndr)) (round (this 'ndr))) )))
                   (newr bigl (list (+ (round abstime) 0.3) "NOTE" (this-f0) chan (round2one (this-or-zero 'sl))
                                  (if (this 'tie) (round (this-sounding-ndr)) (round (this 'ndr))) ))
                   )))
           (incf abstime (this 'ndr)) ))
       (each-note ;---sync track tempo stuff---
        (if (this 'bar) (newr bigl (list (+ (round abstime) 0.15) "BAR" (this 'bar))))
        (newr bigl (list (+ (round abstime) 0.17) "DT" (this :tvec)))
        (incf abstime (this 'ndr))
        )))
    (sort bigl 'compare)))

(defun pdm-make-playlist ()
  (let ((abstime 0)(bigl '())(chan 1) (track-inc 0.0))
    (newr bigl (list abstime "FILETYPE" "pDM_score_file")) ;file type
    (newr bigl (list abstime "VERSION" 1.3)) ;file version
    (newr bigl (list abstime "HEADERSTART" 1)) ;
    (newr bigl (list abstime "SCORENAME" (pathname-name (score-filename *active-score*)))) ;score name
    (if (get-first 'meter) (newr bigl (list abstime "METER" (car (get-first 'meter)) (cadr (get-first 'meter))))) ;meter
    (if (get-first 'mm) (newr bigl (list abstime "TEMPO" (mm-to-microsec (get-first 'mm))))) ;tempo
    (if (and (get-first 'key)(get-first 'modus)) (newr bigl (list abstime "KEY" (get-first 'key) (get-first 'modus)))) ;key
    (newr bigl (list (+ (round abstime) 0.01) "HEADEREND" 1)) ;
    (each-track
     (setq abstime 0)
     (incf track-inc 0.01)
     ;;(incf abstime (get-track-var 'track-delay)) ;gets modulated by tempo variations in pDM - maybe not intended!
     (if (not (string-equal (trackname *this-track*) "sync-track"))
         (then 
          (if (get-track-var 'midi-channel) (setq chan (get-track-var 'midi-channel))) ;get midi channel
          (newr bigl (list abstime "TRACKNAME" (get-track-var 'trackname) chan)) ;trackname
          (newr bigl (list abstime "SYNTH" (synth-symbol-to-name (type-of (get-track-var 'synth))) chan)) ;synth name
          (newr bigl (list abstime "PROGRAM" (get-track-var 'midi-initial-program) chan)) ;inital program 
          (if (get-track-var 'midi-bank-msb) (newr bigl (list abstime "BANKMSB" (get-track-var 'midi-bank-msb) chan))) ;inital bank msb 
          (if (get-track-var 'midi-bank-lsb) (newr bigl (list abstime "BANKLSB" (get-track-var 'midi-bank-lsb) chan))) ;inital bank lsb 
          (if (get-track-var 'midi-pan) (newr bigl (list abstime "PAN" (get-track-var 'midi-pan) chan))) ;inital pan 
          (if (get-track-var 'midi-reverb) (newr bigl (list abstime "REVERB" (get-track-var 'midi-reverb) chan))) ;inital reverb 
          (newr bigl (list abstime "VOL" (get-track-var 'midi-initial-volume) chan)) ;inital volume 
          (newr bigl (list abstime "TRACKDELAY" (get-track-var 'track-delay) chan)) ;track delay
          (each-note ;---all the normal tracks---
           (if (this 'q) (newr bigl (list (+ (round abstime) 0.03) "CHORD" (chordname-to-rootname (this 'q)) (chordname-to-modus (this 'q))))) ;chords
           (when (not (this 'rest))
             (newr bigl (list (+ (round abstime) 0.05 track-inc) "DSL" (this :slvec)))     ; DSL
             (newr bigl (list (+ (round abstime) 0.05 track-inc) "DART" (this :artvec)))   ; DART
             (if (or (first?) (not (prev 'tie)))                                ; NOTE
                 (if (listp (this 'f0))
                     (dolist (f0 (this 'f0))
                       (newr bigl (list (+ (round abstime) 0.055 track-inc) "NOTE" f0 chan (round2one (this-or-zero 'sl))
                                        (if (this 'tie) (round (this-sounding-ndr)) (round (this 'ndr))) )))
                   (newr bigl (list (+ (round abstime) 0.055 track-inc) "NOTE" (this-f0) chan (round2one (this-or-zero 'sl))
                                  (if (this 'tie) (round (this-sounding-ndr)) (round (this 'ndr))) ))
                   )))
           (incf abstime (this 'ndr)) ))
       (each-note ;---sync track tempo stuff---
        (if (this 'bar) (newr bigl (list (+ (round abstime) 0.02) "BAR" (this 'bar))))
        (newr bigl (list (+ (round abstime) 0.04) "DT" (this :tvec)))
        (incf abstime (this 'ndr))
        )))
    (sort bigl 'compare)))
|#

;;include DSL and DART for each note in a chord (don't like it!)
(defun pdm-make-playlist ()
  (let ((abstime 0)(bigl '())(chan 1) (track-inc 0.0))
    (newr bigl (list abstime "FILETYPE" "pDM_score_file")) ;file type
    (newr bigl (list abstime "VERSION" 1.3)) ;file version
    (newr bigl (list abstime "HEADERSTART" 1)) ;
    (newr bigl (list abstime "SCORENAME" (pathname-name (score-filename *active-score*)))) ;score name
    (if (get-first 'meter) (newr bigl (list abstime "METER" (car (get-first 'meter)) (cadr (get-first 'meter))))) ;meter
    (if (get-first 'mm) (newr bigl (list abstime "TEMPO" (mm-to-microsec (get-first 'mm))))) ;tempo
    (if (and (get-first 'key)(get-first 'modus)) (newr bigl (list abstime "KEY" (get-first 'key) (get-first 'modus)))) ;key
    (newr bigl (list (+ (round abstime) 0.01) "HEADEREND" 1)) ;
    (each-track
     (setq abstime 0)
     (incf track-inc 0.01)
     ;;(incf abstime (get-track-var 'track-delay)) ;gets modulated by tempo variations in pDM - maybe not intended!
     (if (not (string-equal (trackname *this-track*) "sync-track"))
         (then 
          (if (get-track-var 'midi-channel) (setq chan (get-track-var 'midi-channel))) ;get midi channel
          (newr bigl (list abstime "TRACKNAME" (get-track-var 'trackname) chan)) ;trackname
          (newr bigl (list abstime "SYNTH" (synth-symbol-to-name (type-of (get-track-var 'synth))) chan)) ;synth name
          (newr bigl (list abstime "PROGRAM" (get-track-var 'midi-initial-program) chan)) ;inital program 
          (if (get-track-var 'midi-bank-msb) (newr bigl (list abstime "BANKMSB" (get-track-var 'midi-bank-msb) chan))) ;inital bank msb 
          (if (get-track-var 'midi-bank-lsb) (newr bigl (list abstime "BANKLSB" (get-track-var 'midi-bank-lsb) chan))) ;inital bank lsb 
          (if (get-track-var 'midi-pan) (newr bigl (list abstime "PAN" (get-track-var 'midi-pan) chan))) ;inital pan 
          (if (get-track-var 'midi-reverb) (newr bigl (list abstime "REVERB" (get-track-var 'midi-reverb) chan))) ;inital reverb 
          (newr bigl (list abstime "VOL" (get-track-var 'midi-initial-volume) chan)) ;inital volume 
          (newr bigl (list abstime "TRACKDELAY" (get-track-var 'track-delay) chan)) ;track delay
          (each-note ;---all the normal tracks---
           (if (this 'q) (newr bigl (list (+ (round abstime) 0.03) "CHORD" (chordname-to-rootname (this 'q)) (chordname-to-modus (this 'q))))) ;chords
           (when (not (this 'rest))
             ;(newr bigl (list (+ (round abstime) 0.05 track-inc) "DSL" (this :slvec)))     ; DSL
             ;(newr bigl (list (+ (round abstime) 0.05 track-inc) "DART" (this :artvec)))   ; DART
             (if (or (first?) (not (prev 'tie)))                                ; NOTE
                 (if (listp (this 'f0))
                     (let ((chord-inc 0.0))
                       (dolist (f0 (this 'f0))
                         (incf chord-inc 0.001)
                         (newr bigl (list (+ (round abstime) 0.05 track-inc chord-inc) "DSL" (this :slvec)))     ; DSL
                         (newr bigl (list (+ (round abstime) 0.05 track-inc chord-inc) "DART" (this :artvec)))   ; DART
                         (newr bigl (list (+ (round abstime) 0.0505 track-inc chord-inc) "NOTE" f0 chan (round2one (this-or-zero 'sl))
                                          (if (this 'tie) (round (this-sounding-ndr)) (round (this 'ndr))) ))))
                   (progn
                     (newr bigl (list (+ (round abstime) 0.05 track-inc) "DSL" (this :slvec)))     ; DSL
                     (newr bigl (list (+ (round abstime) 0.05 track-inc) "DART" (this :artvec)))   ; DART
                     (newr bigl (list (+ (round abstime) 0.0505 track-inc) "NOTE" (this-f0) chan (round2one (this-or-zero 'sl))
                                  (if (this 'tie) (round (this-sounding-ndr)) (round (this 'ndr))) ))
                   ))))
           (incf abstime (this 'ndr)) ))
       (each-note ;---sync track tempo stuff---
        (if (this 'bar) (newr bigl (list (+ (round abstime) 0.02) "BAR" (this 'bar))))
        (newr bigl (list (+ (round abstime) 0.04) "DT" (this :tvec)))
        (incf abstime (this 'ndr))
        )))
    (sort bigl 'compare)))

;get the played duration of current note including ties
(defun this-sounding-ndr ()
  (let ((i *i*)(ndr (this 'ndr)))
    (if (this 'tie)
        (untilexit end
                   (incf i)
                   (cond ((or (this 'rest) (> i (1- (length *v*))))
                          (return-from end nil) )
                         ((iget i 'tie)
                          (incf ndr (iget i 'ndr)) )
                         (t
                          (incf ndr (iget i 'ndr))
                          (return-from end nil) )
                         )))
    ndr))


;returns the value of prop or zero if nil
(defun this-or-zero (prop)
  (or (this prop) 0) )


;prints the eventlist in the proper format
;converts to relative "tick" time in ms
(defun pdm-print-rule-data-stream (stream)
  (let ((bigl (pdm-make-playlist))
        (abstime 0)
        (lastabstime 0))
    (dolist (cmdlist bigl)
      (setq abstime (round (pop cmdlist)))
      (princ (- abstime lastabstime) stream)
      (setq lastabstime abstime)
      (dolist (one cmdlist)
        (cond ((and (arrayp one) (not (stringp one)))
               (dotimes (j (length one))
                 (princ " " stream)(princ (convert-zero (aref one j)) stream) ))
              (t
               (if (floatp one) (setq one (convert-zero one))) ;minimize zero printing
               (princ " " stream)(princ one stream) ))
        )
      (princ ";" stream)(terpri stream)
      )))

;limit to 2 decimals for printing
(defun round2two (nr)
  (/ (round (* nr 100.0)) 100.0) )

(defun round2three (nr)
  (/ (round (* nr 1000.0)) 1000.0) )

(defun round2one (nr)
  (/ (round (* nr 10.0)) 10.0) )

(defun convert-zero (nr)
  (if (zerop nr) 0 nr) )

;;chordname splits - should be moved to initconvert
;works only for string type names
(defun chordname-to-rootname (q)
  (subseq q 0 (- (length q) 3)) )

(defun chordname-to-modus (q)
  (subseq q (- (length q) 3)) )
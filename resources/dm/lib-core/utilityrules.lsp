;;;-*-Mode: LISP; Package: DM -*-
;;
;; ***********************************************************
;;
;; 1986-88/Anders Friberg
;; 9101 cl/af
;; 9702 /vc documented and adapted to new system **** in progress
;; 041109/af addded reset-sound-level
;; 050512/af changed norm-sl to adopt to nominal sound level (nsl)
;; 090420/af added transposition major - minor

(in-package :dm)

;;    rebar ()                
;;    set-all (prop val)
;;    set-all-but-rest (prop val)
;;    add-all (prop val)
;;    add-all-but-rest (prop val)
;;    rem-all (prop)
;;    rem-extra ()
;;    set-all-va-zero ()
;;    set-ndr ()
;;    merge-voices *** commented out
;;    get-first-tempo () * internal ?
;;    set-tempo (mm &key (init-dur? t)) * internal ?
;;    set-key (key modus) * internal ?
;;    set-meter (m1 m2 &key (rebar? t)) * internal ?
;;    trans-octave (nr)
;;    convert-all-to-sharp ()
;;    convert-all-to-flat ()
;;    trans-values (nr)
;;    set-prop-at-bar-and-onset-number (prop val bar onset-nr)
;;    scale-all (factor)
;;    scale-da0 (factor)
;;    scale-dr (factor)
;;    this-ntempo ()   * internal ?
;;    set-this-ntempo (ntempo)   * internal ?
;;    scale-tempo (factor)
;;    scale-dro (factor)
;;    scale-va (factor)
;;    scale-vf (factor)
;;    transpose (semitones)
;;    normalize-sl ()
;;    normalize-dr ()
;;    ndr-norm ()    * internal ?
;;    normalize-dr-bar ()
;;    dr-limits ()
;;    dro-limits ()
;;    rem-dro ()
;;    maximum-span-dr%   * internal ?
;;    check-timing (nr)
;;    mark-ddr% ()     * internal ?
;;    dr-len (bar)
;;    drsum-all-bars ()
;;    total-duration ()
;;    print-total-duration ()    * internal ?
;;    print-drsum ()
;;    print-dr-absolute ()
;;    mark-drabs ()          * internal ?
;;    mark-ndrabs ()      * internal ?
;;    print-ndrabs-tab-ddr% ()   * internal ?
;;    print-ndrabs-tab-soundlevel ()
;;    print-ndrabs-tab-dc ()
;;    print-tab-note-ndr-ddr% ()
;;    print-to-excel ()
;;    princ-excel-float-tab (float)
;;    princ-excel-tab (any)
;;    print-excel-tab-note-ndr-ddr ()
;;    mark-barddr ()
;;    mark-bardr ()
;;    print-barddr ()
;;    print-bardr ()
;;    make-bar-mel () * commented out


;;;---------------- bar tricks--------------------------------

;; ---------------------
;;   REBAR remove bars
;; ---------------------
;;
(defun rebar ()
 (each-note
    (rem-this 'bar) )
  (mark-bar)
  ;(redraw-music-windows)
  )

;;; (defun rebar-mf ()
;;;  (each-note
;;;     (rem-this 'bar) )
;;;  (mark-bar-mf) )

;;;---------------- beat tricks--------------------------------

;; ---------------------
;;   mark beat
;; ---------------------
;;

#|
(defun mark-beat ()
  (let ((beat-value 4)
        ack-value)
    (each-note             ;mark beat
      (when (this 'meter) 
         (setq beat-value (/ 1 (cadr (this 'meter)))) )
      (when (this 'bar) 
         (setq ack-value 0))
      (when (zerop (mod ack-value beat-value))
        (set-this :beat t)
        ;(print-ll *i* "  " (this 'n))
        )
      (incf ack-value (get-note-value-fraction *i*))
     )))
|#

;new version with meter templates
;130108/af
(defun mark-beat ()
  (let ((beat-value 4)
        ack-value)
    (each-note             ;mark beat
      (when (this 'meter) 
         (setq beat-value (beat-fraction-from-meter (this 'meter))))
      (when (this 'bar) 
         (setq ack-value 0))
      (when (zerop (mod ack-value beat-value))
        (set-this :beat t)
        ;(print-ll *i* "  " (this 'n))
        )
      (incf ack-value (get-note-value-fraction *i*))
     )))

(defun beat-fraction-from-meter (meter)
  (cond
    ((equal meter '(4 4)) 1/4)
    ((equal meter '(2 2)) 1/2)
    ((equal meter '(4 2)) 1/2)
    ((equal meter '(2 4)) 1/4)
    ((equal meter '(3 4)) 1/4)
    ((equal meter '(6 8)) 3/8)
    ((equal meter '(9 8)) 3/8)
    ((equal meter '(12 8)) 3/8)
   ))

;mark the middle of bar in the cases 4/4, 4/2 and 12/8
;otherwise nothing
(defun mark-half-bar ()
  (let (ack-value 
        beat-value
        (mark-half-bar-p nil) )
    (each-note             ;mark beat
     ;get new meter and check if half-beat
     (when (and (this 'meter) (half-bar-fraction-from-meter (this 'meter)))
       (setq mark-half-bar-p t)
       (setq beat-value (half-bar-fraction-from-meter (this 'meter))))
     (when (and (this 'meter) (not (half-bar-fraction-from-meter (this 'meter))))
       (setq mark-half-bar-p nil) )
     ;reset counter at bar
     (when (this 'bar) 
       (setq ack-value 0))
     ;check if half-bar
     (when (and mark-half-bar-p (zerop (mod ack-value beat-value)))
       (set-this :half-bar t)
       ;(print-ll *i* "  " (this 'n))
       )
     (incf ack-value (get-note-value-fraction *i*))
     )))

(defun half-bar-fraction-from-meter (meter)
  (cond
    ((equal meter '(4 4)) 2/4)
    ((equal meter '(2 2)) nil)
    ((equal meter '(4 2)) 2/2)
    ((equal meter '(2 4)) nil)
    ((equal meter '(3 4)) nil)
    ((equal meter '(6 8)) nil)
    ((equal meter '(9 8)) nil)
    ((equal meter '(12 8)) 6/8)
   ))

(defun mark-beat-number-in-measure ()
  (let ((beat-value 4)
        ack-value
        beat-nr)
    (each-note             ;mark beat
      (when (this 'meter) 
         (setq beat-value (/ 1 (cadr (this 'meter)))) )
      (when (this 'bar) 
        (setq ack-value 0)
        (setq beat-nr 1) )
      (when (zerop (mod ack-value beat-value))
        (set-this :beat (1+ (round (/ ack-value beat-value))))
        (incf beat-nr)
        ;(print-ll *i* "  " (this 'n))
        )
     (incf ack-value (get-note-value-fraction *i*))
     )))


;; ---------------------
;;   mark offbeat
;; ---------------------
;;

(defun mark-offbeat ()
  (let ((beat-value 4) ack-value)
    (each-note             ;mark beat
      (when (this 'meter) 
         (setq beat-value (/ 1 (cadr (this 'meter)))) )
      (when (this 'bar) 
         (setq ack-value 0))
      (when (= (mod ack-value beat-value) 
               (/ beat-value 2) )
        (set-this :offbeat t)
        ;(print-ll *i* "  " (this 'n))
        )
      (incf ack-value (get-note-value-fraction *i*))
      )))


;;;---------------- set properties for all notes --------------------

(defun set-all (prop val)
 (each-note (set-this prop val)) )

(defun set-all-but-rest (prop val)
  (each-note-if (this prop) (not (this 'rest)) (set-this prop val)) )

(defun add-all (prop val)
  (each-note-if (this prop) (add-this prop val)) )

(defun add-all-but-rest (prop val)
  (each-note-if (this prop) (not (this 'rest)) (add-this prop val)) )

(defun rem-all (prop)
 (each-note (rem-this prop)) )

(defun rem-extra ()
  (each-note
    (rem-this 'dc)
    (rem-this 'va)
    (rem-this 'vf)
))

(defun set-ndr ()
  (each-note (set-this 'dr (this 'ndr))) )

(defun set-all-va-zero () (set-all 'va 0))

#|
;only if same note values in all voices
;the result in the first voice in *vlist*        
(defun merge-voices ()
   (let ((vres (car *vlist*)))
    (p-each-note
      (let ((val (note-to-notevalue (v-this vres 'n))))
       (v-set-this vres 'n (cons (mapcar '(lambda ((v . (tone . val))) tone) (p-this 'n)) val))
       (v-set-this vres 'f0 (mapcar '(lambda ((v . f0)) f0) (p-this 'f0)))
       ))))
|#       

;;0009/af
;;a chord in one voice is distributed to all voices if the have the
;;same onset position
(defun distribute-chord-analysis ()
     (p-each-note
      (when (p-this 'q)
        (p-set-this 'q (cdr (first (p-this 'q)))))))

;;0009/af as above for phrases
(defun distribute-phrase-analysis ()
     (p-each-note
      (when (p-this 'phrase-start)
        (p-set-this 'phrase-start (cdr (first (p-this 'phrase-start)))))
      (when (p-this 'phrase-end)
        (p-set-this 'phrase-end (cdr (first (p-this 'phrase-end)))))))

;;0911/af
;;add a number to all phrase levels
(defun add-all-phrase-levels (nr)
     (each-note
      (when (this 'phrase-start)
        (set-this 'phrase-start (mapcar #'(lambda (x) (+ nr x)) (this 'phrase-start))))
      (when (this 'phrase-end)
        (set-this 'phrase-end (mapcar #'(lambda (x) (+ nr x)) (this 'phrase-end))))))


;set all sl and nsl to zero
;useful after midifile input
(defun reset-sound-level ()
  (each-note
   (if (this 'sl) (set-this 'sl 0))
   (if (this 'nsl) (set-this 'nsl 0))
   ))

;-------- score editing --------------------------

;returns the first mm it finds
;behsvs ej anvSnd get-first istSllet
(defun get-first-tempo ()
  (let (mm)
  (each-note-if
    (this 'mm)
    (then (setq mm (this 'mm)))
    (return)
    )
  mm))

(defun set-tempo-init (mm)
  ;(rem-all 'mm)
  (set-first 'mm mm)
  (set-dur-from-tempo)
  )

;scale the performance as well
;init-dur  if no old tempo value
(defun set-tempo (mm &key (init-dur? t))
   (let* ((oldmm (get-first 'mm))
          (scale (/ oldmm (float mm))) )
      (cond
       ((and oldmm (not (= oldmm mm)))
          (progn
            (each-note
              (set-this 'dr (* (this 'dr) scale))
              (set-this 'ndr (* (this 'ndr) scale))
              (if (this 'dro)(set-this 'dro (* (this 'dro) scale)))
              (if (this 'mm) (set-this 'mm (/ (this 'mm) scale)))
              )
            (set-first 'mm mm) ))
       ((not oldmm)
        (set-tempo-init mm) )
       (t (warn "no tempo was set"))
       )))

(defun scale-score-tempo (factor)
  (set-tempo (* (get-first 'mm) factor)) )
                   

(defun set-key (key modus)
  (rem-all 'key)
  (rem-all 'modus)
  (set-first 'modus modus)
  (set-first 'key key)
  )

(defun set-meter (m1 m2 &key (rebar? t))
  (rem-all 'meter)
  (set-first 'meter (list  m1 m2))
  (if  rebar? (rebar))
  )

;set a prop in the first note of each track
(defun set-first (prop value)
   (each-segment-if
     (first?)
     (then
       (set-this prop value)
       (exit-track) )))

;gets a prop from the first note of the first track
(defun get-first (prop)
   (let ((value))
      (block loop
        (each-segment-if
          (first?)
          (then
            (setq value (this prop))
            (return-from loop value) )))
      value))

;remove a property on the first note of each track
(defun rem-first (prop)
   (each-segment-if
     (first?)
     (then
       (rem-this prop)
       (exit-track) )))

;-----------------------


;;remove all the lower chord notes
;;works only for f0
(defun keep-only-top-note-in-chords ()
  (each-note-if
   (this 'f0)
   (listp (this 'f0))
   (then
    (set-this 'f0 (apply 'max (this 'f0)))
    )))


;;works with chords
(defun convert-all-to-sharp ()
  (each-note-if
     (not (this 'rest))
     ;(flat?)
     (then
      (set-this 'n (flat-note-to-sharp (this 'n)))
        )))
(defun convert-all-to-flat ()
  (each-note-if
     (not (this 'rest))
     ;(sharp?)
     (then
      (set-this 'n (sharp-note-to-flat (this 'n))) ))) 


;change the note values
(defun trans-values (nr)
 (each-note-if
   (not (this 'rest))
   (set-this 'n
     (to-note (note-to-tone (this 'n))
              (note-to-octave (this 'n))
              (truncate (* nr (note-to-notevalue (this 'n)))))))
 (each-note-if
    (this 'rest)
    (set-this 'n
     (cons () (truncate (* nr (note-to-notevalue (this 'n)))))))
  )
 
#|
;sets the property in all voices according to *cur-notes*
;onset and bar number start at 1
(defun set-prop-at-bar-and-onset-number (prop val bar onset-nr)
  (let ((i 0) (start-count nil))
    (p-each-note
      (if (and (p-this 'bar) 
               (= (cdar (p-this 'bar)) bar) )
        (setq start-count t) )
      (if start-count (incf i))
      (if (and start-count (= i onset-nr))
        (progn
          ;(print "hittat")
          (p-set-this prop val)
          (return) ))
      )))
|#    

;fix waiting for editmusicwindow
(defun set-track-nr-active-p (nr value)
   (let ((track (nth (1- nr) (track-list *active-score*))))
      (setf (active-p track) value)
      ))

;------ transpose notes-----------------------------------------

#|       
;all-tracks? not used- always only active tracks
(defun trans-octave (nr &key (all-tracks? nil))
 (each-note-if
  (not (this 'rest))
  (then
   (set-this 'n
     (to-note (note-to-tone (this 'n))
              (+ nr (note-to-octave (this 'n)))
              (note-to-notevalue (this 'n)) ))
   (set-this  'f0 
      (+ (this 'f0) (* 12 nr)) )
 )))
;with chords
(defun trans-octave (nr)
   (each-note-if
    (not (this 'rest))
    (then
     (let ((n (this 'n)))
        (cond
         ((listp (car n))
          (set-this 'n
            (cons
             (mapcar #'(lambda (to) 
                         (to-toneoctave 
                           (toneoctave-to-tone to)
                           (+ (toneoctave-to-octave to) nr)))
               (car n))
             (cdr n))))
         (t
          (set-this 'n
            (to-note (note-to-tone n)
              (+ nr (note-to-octave n))
              (note-to-notevalue n) ))
          (set-this  'f0 
            (+ (this 'f0) (* 12 nr)) )
          ))))))
|#
;with proper recomputing of f0 values
(defun trans-octave (nr)
   (each-note-if
    (not (this 'rest))
    (then
     (let ((n (this 'n)))
        (cond
         ((listp (car n))
          (set-this 'n
            (cons
             (mapcar #'(lambda (to) 
                         (to-toneoctave 
                           (toneoctave-to-tone to)
                           (+ (toneoctave-to-octave to) nr)))
               (car n))
             (cdr n))))
         (t
          (set-this 'n
            (to-note (note-to-tone n)
              (+ nr (note-to-octave n))
                     (note-to-notevalue n) )))))
     (set-this 'f0 (note-to-f0 (this 'n))) ;recompute f0 values         
          )))


;the notes will not change
(defun transpose (semitones)
  (setq semitones (round semitones))
  (each-note-if
   (not (this 'rest))
   (then
    (if (listp (this 'f0))
            (let ((f0-temp '()))
              (set-this 'f0 
                        (dolist (one (this 'f0) (nreverse f0-temp)) (push (+ one semitones) f0-temp))
                        ))
         (set-this  'f0 (+ (this 'f0) semitones))
      ))))


;----transpose major-minor-----

; applies to notes as well as f0
; do not consider right enharmonic spelling
; will not generate proper dominants from major to minor, rather it will be a modal character

(defun transpose-from-major-to-minor ()
  (let ((key-nr 0))
  (each-note
   (if (this 'key) (setq key-nr (tone-to-tonnr (this 'key))))
   (when (not (this 'rest))
     (let ((maj3 (mod (+ key-nr 4) 12))
           (maj6 (mod (+ key-nr 9) 12))
           (maj7 (mod (+ key-nr 11) 12)) )
       (this-replace-tone maj3 (1- maj3))
       (this-replace-tone maj6 (1- maj6))
       (this-replace-tone maj7 (1- maj7))
       (set-this 'f0 (note-to-f0 (this 'n))) ;recompute f0 values
       )))
    (set-first 'modus "min")
    ))
        
(defun transpose-from-minor-to-major ()
  (let ((key-nr 0))
  (each-note
   (if (this 'key) (setq key-nr (tone-to-tonnr (this 'key))))
   (when (not (this 'rest))
     (let ((min3 (mod (+ key-nr 3) 12))
           (min6 (mod (+ key-nr 8) 12))
           (min7 (mod (+ key-nr 10) 12)) )
       (this-replace-tone min3 (1+ min3))
       (this-replace-tone min6 (1+ min6))
       (this-replace-tone min7 (1+ min7))
       (set-this 'f0 (note-to-f0 (this 'n))) ;recompute f0 values
       )))
    (set-first 'modus "maj")
))
  

;only within macro call
;replaces all occurences of old-tonnr with new-tonnr
;only notes no f0
(defun this-replace-tone (old-tonnr new-tonnr)
    (let ((note (this 'n)))
     (if (not (listp (car note)))
         (set-this 'n (cons (replace-tonoctave-one (car note) old-tonnr new-tonnr) (cdr note)))
       (let ((new-note-l '()))
         (dolist (n (car note) new-note-l) (push (replace-tonoctave-one n old-tonnr new-tonnr) new-note-l))
         (set-this 'n (cons (nreverse new-note-l) (cdr note)))
         ))))

;replaces all occurences of old-tonnr with new-tonnr
;only notes no f0
(defun replace-tone (old-tonnr new-tonnr)
  (each-note-if
   (not (this 'rest))
   (then
    (let ((note (this 'n)))
     (if (not (listp (car note)))
         (set-this 'n (cons (replace-tonoctave-one (car note) old-tonnr new-tonnr) (cdr note)))
       (let ((new-note-l '()))
         (dolist (n (car note) new-note-l) (push (replace-tonoctave-one n old-tonnr new-tonnr) new-note-l))
         (set-this 'n (cons (nreverse new-note-l) (cdr note)))
         ))))))

;subroutine to above
;changes  the tone of tonoctave if the tonnr within one octave corresponds to old-tonnr
(defun replace-tonoctave-one (tonoctave old-tonnr new-tonnr) 
  (let ((tone (toneoctave-to-tone tonoctave))
        (octave (toneoctave-to-octave tonoctave)) )
    (if (= (tone-to-tonnr tone) old-tonnr)
        (setq tone  (tonnr-to-tone new-tonnr)) )
    (to-toneoctave tone octave) ))

(defun replace-tonoctave-one (tonoctave old-tonnr new-tonnr) 
  (let ((tone (toneoctave-to-tone tonoctave))
        (octave (toneoctave-to-octave tonoctave)) )
    (when (= (tone-to-tonnr tone) old-tonnr)
      ;(print-ll "old " old-tonnr " new " new-tonnr)
      (setq tone  (tonnr-to-tone new-tonnr))
      (when (> new-tonnr 11) (setq octave (1+ octave))) ;fix 
      (when (< new-tonnr 0) (setq octave (1- octave)))
      )
    (to-toneoctave tone octave) ))


;----------- scaling ----------------------------

(defun scale-all (factor)
 (scale-dr factor)
 (scale-da0 factor)
 (scale-dro factor) 
 (scale-va factor) 
)

(defun scale-da0 (factor)
  (each-note-if
    (not (this 'rest))
    (then
       (set-this 'da0
         (round (* factor (this 'da0)))
          ))))

(defun scale-dr (factor)
  (each-note
    (set-this 'dr (+ (this 'ndr) (* factor (- (this 'dr) (this 'ndr))))) )
   )

;;--- scale tempo deviations -----
(defun scale-delta-tempo (factor)
  (each-note
    ;(print-ll "factor " factor " old ntempo " (this-ntempo))
    (set-this-ntempo (+ (* factor (- (this-ntempo) 1.0)) 1))
    ;(print-ll "      new ntempo " (this-ntempo))
   ))

(defun this-ntempo ()
  (/ (this 'ndr) (this 'dr)) )

(defun set-this-ntempo (ntempo)
  (set-this 'dr (/ (this 'ndr) ntempo) ))

;---

(defun scale-dro (factor)
  (each-note-if
    (this 'dro)
    (set-this 'dro (* factor (this 'dro))) 
   ))

(defun scale-va (factor)
  (each-note-if
    (this 'va)
    (set-this 'va (round (* factor (this 'va))))
   ))

(defun scale-vf (factor)
  (each-note-if
    (this 'vf)
    (set-this 'vf (round (* factor (this 'vf))))
   ))

;used by emotion rules
;;include also dro
;;030930/af
(defun scale-duration (factor) 
  (each-note
   (set-this 'dr (* (this 'dr) factor))
   (if (this 'dro) (set-this 'dro (* (this 'dro) factor)))
   ))
(defun scale-duration-only-dr (factor) 
  (each-note
   (set-this 'dr (* (this 'dr) factor))
   ))


;same but with tempo
(defun scale-tempo (factor)
  (scale-duration (/ 1.0 factor)) )
(defun scale-tempo-only-dr (factor)
  (scale-duration-only-dr (/ 1.0 factor)) )


;used by emotion rule
(defun set-sound-level (value) 
   (each-note-if (not (this 'rest)) (then (add-this 'sl (* 3 value)))))

;similar but with decibels
(defun scale-sound-level (db) 
   (each-note-if (not (this 'rest)) (then (add-this 'sl db))))



;---------- normalize --------------------------------------


;change the mean value of sl to be zero in each track
;;;(defun normalize-sl ()
;;;   (each-track
;;;     (let ((sltot 0.0)
;;;           (nr 0))
;;;        (each-note                                ;compute sltot
;;;          (when (not (this 'rest))
;;;             (incf sltot (this 'sl))
;;;             (incf nr) ))
;;;        (let ((addval (/ sltot nr)))
;;;           (each-note-if
;;;             (not (this 'rest))
;;;             (then
;;;               (set-this 'sl (- (this 'sl) addval))
;;;              ))))))

;;normalize relatively nominal sl
(defun normalize-sl ()
   (each-track
    (let ((sltot 0.0)
          (nsltot 0.0)
          (nr 0) )
      (each-note                                ;compute sltot
       (when (not (this 'rest))
         (incf sltot (this 'sl))
         (if (this 'nsl) (incf nsltot (this 'nsl)))
         (incf nr) ))
      (let ((addval (- (/ sltot nr)(/ nsltot nr))))
        (each-note-if
         (not (this 'rest))
         (then
          (set-this 'sl (- (this 'sl) addval))
          ))))))

;maximize the sl corresponding to velocity 127
(defun maximize-sl ()
   (each-track
    (let ((sl-limit (get-max-sl (get-track-var 'synth)))
          (sl-max -100) )
      (each-note-if
       (this 'sl)
       (> (this 'sl) sl-max)
       (then
        (setq sl-max (this 'sl)) 
        ))
      ;(print-ll "sl-limit " sl-limit "  sl-max " sl-max)
      (each-note-if
       (this 'sl)
       (then
        (add-this 'sl (- sl-limit sl-max))
        )))))
      
      
(defun normalize-dr ()
 (each-track
  (let ((ddrtot 0.0)
        (drtot 0.0) )
  (each-note
     (setq ddrtot (+ (- (this 'dr) (this 'ndr))
                     ddrtot ))
     (setq drtot (+ (this 'dr) drtot)) )
  (let ((fact (/ (- drtot ddrtot) drtot)))
    (each-note
       (set-this 'dr (* fact (this 'dr)))
       )))))

;normalize the nominal duration so the total length is same as for the sum of 'dr
;will not change the tempo property
(defun normalize-ndr ()
 (each-track
  (let ((drtot 0.0)
        (ndrtot 0.0) )
  (each-note
     (incf drtot (this 'dr))
     (incf ndrtot (this 'ndr)) )
  (let ((fact (/ drtot ndrtot)))
    (each-note
       (set-this 'ndr (* fact (this 'ndr)))
       )))))



(defun normalize-dr-bar ()
 (let ((fact))
  (mark-barddr)
  (mark-bardr)
  (each-note
    (if (this 'barddr)
        (setq fact (/ (- (this 'bardr) (this 'barddr)) (this 'bardr))) )
    (set-this 'dr (* fact (this 'dr))) ))
 (rem-all 'barddr)
 (rem-all 'bardr)
 )

;;---- check and adjust timing errors --------------

(defun dr-limits ()
  (each-note-if
    (< (this 'dr) 10)
    (then
     (print-this "Warning: DR less than 10 ms" 'dr nil)
     (when (minusp (this 'dr))
       (print-this "Warning: DR negative - setting DR to 1 -" 'dr nil)  
       (set-this 'dr 1.0) )
    )))

(defun dro-limits ()
  (each-note-if
    (this 'dro)
    (< (- (this 'dr) (this 'dro)) 10)
    (then
       (print-this "Warning: duration (dr-dro) shorter than 10 ms - setting it to 10 ms - " 'dro nil)
       (set-this 'dro (round (- (this 'dr) 10)))
    )))

(defun rem-dro ()
  (each-note
    (if (this 'tie)
        (then (rem-this 'dro)
              (print-this "Warning: dro on a tied note - removing dro " nil nil) ))
    ))

;---- statistics ----------------------------------------

(defun print-average-ddr ()
  (let ((mean-ddr 0.0) (n 0))
    (each-note
     (incf mean-ddr (- (this 'dr) (this 'ndr)))
     (incf n) )
    (setq mean-ddr (/ mean-ddr n))
    (print-ll "mean-ddr = " mean-ddr " n = " n)
    ))

(defun print-standard-deviation-ddr ()
  (let ((s1 0.0) (s2 0.0) (n 0))
    (each-note
     (incf s1 (expt (- (this 'dr) (this 'ndr)) 2))
     (incf s2 (- (this 'dr) (this 'ndr)))
     (incf n) )
    (let ((sd-ddr (sqrt (/ (- s1 (/ (expt s2 2) n)) (1- n)))))
      (print-ll "sd-ddr = " sd-ddr " n = " n)
      )))

(defun standard-deviation-ddr ()
  (let ((s1 0.0) (s2 0.0) (n 0))
    (each-note
     (incf s1 (expt (- (this 'dr) (this 'ndr)) 2))
     (incf s2 (- (this 'dr) (this 'ndr)))
     (incf n) )
    (let ((sd-ddr (sqrt (/ (- s1 (/ (expt s2 2) n)) (1- n)))))
      sd-ddr
    )))

;---- durations ----------------------------------------

(defun maximum-span-dr% ()
  (let ((max -100)(min 100)(ddr% 0))
  (each-note-if
    (not (this 'rest))
    (then
      (setq ddr% (* (/ (- (this 'dr) (this 'ndr)) (this 'ndr)) 100.))
      (cond
       ((> ddr% max)
        (setq max ddr%) )
       ((< ddr% min)
        (setq min ddr%) ))))
  (- max min)))

#|
(defun check-timing (nr)
 (for (i 1 1 nr)
   (dr-len i) ))
|#
 
(defun mark-ddr% ()
  (each-note
    (set-this :ddr%
       (* (/ (- (this 'dr) (this 'ndr)) (this 'ndr)) 100.))
    ))

#| didn't compile in ACL 
;print the total duration up to bar #
;just for testing
(defun dr-len (bar)
  (let ((drsum 0))
   (each-voice
    (block count
     (each-note
        (if (and (this 'bar) (= (this 'bar) bar))
            (return-from count) )
        (setq drsum (+ drsum (this 'dr))) ))
    (print-ll "drsum up to bar# " bar " = " drsum)
    (setq drsum 0)
         )))
|#

#|
(defun drsum-all-bars ()
 (let ((maxbar))
  (each-note-if (this 'bar) (setq maxbar (this 'bar)))
  (for (i 1 1 maxbar)
    (dr-len i) )))
|#

(defun total-duration ()
 (let ((drtot 0)(drtotl))
  (each-track
    (setq drtot 0)
    (each-note
      (setq drtot (+ drtot (this 'dr))) )
    (newr drtotl drtot)
    )
  (let ((max (car drtotl)))
    (mapc '(lambda (dr) (if (> dr max) (setq max dr)))
          drtotl )
    max )))

(defun print-total-duration ()
 (let ((drtot 0)(drtotl))
  (each-track
    (setq drtot 0)
    (each-note
      (setq drtot (+ drtot (this 'dr))) )
    (newr drtotl drtot)
    )
  (print drtotl) ))

;prints the sum of the duration in each voice for each new note
(defun print-drsum ()
  (let ((drsum (make-array (list (length *vlist*)) :initial-element 0)))
  (print drsum)
  (p-each-note
      ;print the voice and number
        (mapc #'(lambda (pair)
                (prin1-ll " " (car pair) " " (cdr pair)) ) 
              *cur-notes* )
      ;print drsum up to these notes rounded in ms
        ;(outpos 30)
        (mapc #'(lambda (pair)
                (prin1-ll " "(round (aref drsum (vname-to-i (car pair))))) ) 
              *cur-notes* )
        (terpri)
      ;compute the new drsum
        (mapc #'(lambda (pair)
                (vset drsum (vname-to-i v) 
                            (+ (cdr (v-iget (cons (car pair) (cdr pair)) 'dr))
                               (aref drsum (vname-to-i (car pair))) ))) 
              *cur-notes* )
         )))

(defun print-dr-absolute ()
  (each-track
    ;(print (car *vlist*))
    (let ((drsum 0)(bar 1))
      (each-note
        (if (this 'bar) (setq bar (this 'bar)))
        (print-ll "time " (round drsum) " bar " bar " note " (this 'n))
        (setq drsum (+ drsum (this 'dr)))
        ))))

(defun mark-drabs ()
  (each-track
    (let ((drsum 0))
      (each-note
        (set-this :drabs drsum)
        (setq drsum (+ drsum (this 'dr)))
        ))))

(defun mark-ndrabs ()
  (each-track
    (let ((drsum 0))
      (each-note
        (set-this :ndrabs drsum)
        (setq drsum (+ drsum (this 'ndr)))
       ))))

;;mark absolute onset and offset times in seconds
;;**** note: tied notes not considered for offset calc
(defun mark-onset-offset-time ()
  (each-track
    (let ((drsum 0))
      (each-note
       (set-this :onset (/ drsum 1000.0))
       (set-this :offset (/ (+ drsum (if (this 'dro) (- (this 'dr) (this 'dro)) (this 'dr))) 1000.0))
        (setq drsum (+ drsum (this 'dr)))
       ))))


;--notes per second----

;;from first sounding note to last not including last
;,disregarding ties
(defun print-nps-all-tracks-ndr ()
  (mark-ndrabs)
  (each-track
   (let ((drtot 0)
         (nofnotes 0) )
     (each-note-if
      (not (this 'rest))
      (not (and (not (first?)) (prev 'tie))) ;not on tied notes
      (then
       (setq drtot (this :ndrabs))
       (setq nofnotes (1+ nofnotes))
       ))
     (let ((first-note nil))
       (each-note-if
        (not first-note)
        (then 
         (if (this 'rest) (setq drtot (- drtot (this 'ndr))))
         (if (not (this 'rest)) (exit-track))
         )))
     (setq nofnotes (1- nofnotes))
     (print-ll "nofnotes " nofnotes " drtot " drtot " nps " (* (/ nofnotes drtot) 1000.0))
     )))

(defun print-nps-all-tracks-dr ()
  (mark-drabs)
  (each-track
   (let ((drtot 0)
         (nofnotes 0) )
     (each-note-if
      (not (this 'rest))
      (not (and (not (first?)) (prev 'tie))) ;not on tied notes
      (then
       (setq drtot (this :drabs))
       (setq nofnotes (1+ nofnotes))
       ))
     (let ((first-note nil))
       (each-note-if
        (not first-note)
        (then 
         (if (this 'rest) (setq drtot (- drtot (this 'dr))))
         (if (not (this 'rest)) (exit-track))
         )))
     (setq nofnotes (1- nofnotes))
     (print-ll "nofnotes " nofnotes " drtot " drtot " nps " (* (/ nofnotes drtot) 1000.0))
     )))


;compute for all tracks including sync track
;works only for ndr
;probably used for cortex 2011 paper (?)
(defun print-nps-all-tracks-sync-ndr ()
  (add-one-track *active-score* (sync-make-mel))
  (print-nps-all-tracks-ndr)
  (remove-one-track *active-score* (1- (length (track-list *active-score*))))
  )



;---------------------------

;mark duration deviation for each bar
(defun mark-barddr ()
  (each-note-if
       (this 'bar)
       (i?next *i* 'bar)
       (then
         (set-this 'barddr (- (drsum *i* (i?next *i* 'bar))
                              (ndrsum *i* (i?next *i* 'bar)) 
                              )))))

(defun mark-bardr ()
  (each-note-if
       (this 'bar)
       (i?next *i* 'bar)
       (then
         (set-this 'bardr (drsum *i* (i?next *i* 'bar)))
         )))

(defun print-bardr ()
   (each-note-if
    (this 'bar)
    (i?next *i* 'bar)
    (print-ll "bar " (this ''bar) "  barddr "     ; in this point the code is broken !!! ******
     ;; (this 'bar) is not correct, there was an ACII mess in the original file !
     (- (drsum *i* (i?next *i* 'bar))
        (ndrsum *i* (i?next *i* 'bar)) ))
    ))

#|
(defun make-bar-mel ()
 (let ((v8-i 0) (ton) (vsym 'v8)(vl))
  (each-note-if
   (this 'bar)
   (i?next *i* 'bar)
   (then
     (setq ton (getsym-tone (vsym-to-vnr vsym) (incf v8-i)))
     (symbol-plist ton nil)
     (setf (get ton 'dr) (drsum *i* (i?next *i* 'bar)))
     (setf (get ton 'ndr) (ndrsum *i* (i?next *i* 'bar)))
     (setf (get ton 'n) (this 'n))
     (newr vl ton) ))
  (set vsym vl)
  ))
|#
              

;;--------------------------------
;;print prop tab-delimited
;;--------------------------------


;;print a list of prop
;; stream t prints to screen
;; prop names in the first row if header? true
(defun print-prop-list-tabdelimited (prop-list header? stream)
  (print prop-list)
  (when header?
    (dolist (prop prop-list)
      (format stream "~A~A" prop #\Tab) )
    (fresh-line stream) )
  (each-note
   (dolist (prop prop-list)
     (format stream "~A~A" (this prop) #\Tab) )
   (fresh-line stream)
   ))


(defun print-ndrabs-tab-ddr% ()
  (mark-ndrabs)
  (mark-ddr%)
    (format t "~1&~A~A~A" "ndrabs" #\tab "ddr% * 10")
  (each-note
    (format t "~1&~A~A~A" (round (this :ndrabs)) #\tab (round (* 10 (this :ddr%))))
    )
  (rem-all :ndrabs)
  (rem-all :ddr%)
  )
(defun print-ndrabs-tab-soundlevel ()
  (mark-ndrabs)
    (format t "~1&~A~A~A" "ndrabs" #\tab "soundlevel")
  (each-note
    (format t "~1&~A~A~A" (round (this :ndrabs)) #\tab (or (this-l0) ""))
    )
  (rem-all :ndrabs)
  )
(defun print-ndrabs-tab-dc ()
  (mark-ndrabs)
    (format t "~1&~A~A~A" "ndrabs" #\tab "cent")
  (each-note
    (format t "~1&~A~A~A" (round (this :ndrabs)) #\tab (this 'dc))
    )
  (rem-all :ndrabs)
  )
(defun print-tab-note-ndr-ddr% ()
  (mark-ddr%)
    (format t "~1&~A~A~A~A~A" "note" #\tab "ndr" #\tab "ddr% * 10")
  (each-note
    (format t "~1&~A~A~A~A~A"
            (this 'n) #\tab (round (this 'ndr)) #\tab (round (* 10 (this :ddr%))))
    )
  (rem-all :ddr%)
  )

(defun print-to-excel ()
  (let ((va 0)(vf 0)(dc 0))
  (mark-ddr%)
  (mark-ndrabs)
    (format t "~1&~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A" "note" #\tab "f0" #\tab "ndrabs" #\tab
            "ddr%" #\tab "ndr" #\tab "dr" #\tab "soundlevel" #\tab "droff" #\tab "va" #\tab "vf" #\tab "dcent")
  (each-note
    (if (this 'va) (setq va (this 'va)))
    (if (this 'vf) (setq vf (this 'vf)))
    (if (this 'dc) (setq dc (this 'dc)))
    (format t "~1&~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A" (this 'n) #\tab (or (this 'f0) " ") #\tab (this :ndrabs) #\tab
            (this :ddr%) #\tab (this 'ndr) #\tab (this 'dr) #\tab (or (this-l0) " ") #\tab (or (this 'dro) " ") #\tab
            (or (this 'va) va) #\tab (or (this 'vf) vf) #\tab (or (this 'dc) dc) )
    )
  (rem-all :ndrabs)
  (rem-all :ddr%)
  ))

;;-- print for excel input -----
;; with , instead of .

(defun princ-excel-float-tab (float)
  (multiple-value-bind
    (int rest) (truncate float)
    (if (minusp float) (princ "-"))
    (princ (abs int))(princ ",")(format t "~4,'0D" (abs (round (* 10000 rest))))
    (format t "~A" #\tab)
    ))

(defun princ-excel-tab (any)
  (princ any) (format t "~A" #\tab))

(defun print-excel-tab-note-ndr-ddr ()
  (format t "~1&~A~A~A~A~A" "note" #\tab "ndr" #\tab "dr/ndr")
  (each-note
    (terpri)
    (princ (car (this 'n)))(princ " ")(princ (cdr (this 'n)))(if (this 'dot) (princ "."))
    (format t "~A" #\tab)
    (princ-excel-float-tab (this 'ndr))
    (princ-excel-float-tab (/ (this 'dr)(this 'ndr)))
    )
  )


;;--------------------------------
;; batch conversion of files from mus to MIDI 
;;--------------------------------

;converts all ".mus" files in a directory to midi files
(defun mus-to-midi-file-batch ()
  (let ((dir (ask-user-for-directory)))
    (when dir
      (dolist (fpath (directory dir))
        (when (string-equal (pathname-type fpath) "mus")
          (print fpath)
          (load-score-fpath fpath)
         (save-performance-midifile0-fpath (merge-pathnames (make-pathname :type "mid") fpath))
          )))))
         

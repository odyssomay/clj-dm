
;;9202 cl /af

;;Apply a ritard in the end and accelerando in the beginning of each phrase.
;;The music must contain boundary marks in the following way:
;; On the first note of each phrase:
;;    boundary-start <boundary level list>
;; On the last note of each phrase:
;;    boundary-end <boundary level list>
;; The level is the hierarchical phrase level.
;; 7 is the lowest level, typically small motives 3-5 notes
;; 6 is the next level above
;; and so on

;;971117/af converted to DM 2


(in-package :dm)
(export '())

;;local vars to be used only within the rule
(defvar *boundary-start-name*)
(defvar *boundary-end-name*)
(setq *boundary-start-name* 'phrase-start)
(setq *boundary-end-name* 'phrase-end)

;main function

#|
(defun phrase-arch 
       (quant &key (shape :x2) (power 2) (ampscale 1) (boundlevel 7)
              (nextboundscale 1) (2nextboundscale 1) (turnpos 2) (lastnote 1) (startqscale 1))
  (if (not (check-for-phrase-marks))
    (print-ll "Phrase-arch : no phrase marks - skipping rule")
    (progn 
      (phrase-arch-mark-ddr 
       (abs quant) boundlevel :shape shape :power power :ampscale ampscale 
       :nextboundscale nextboundscale :2nextboundscale 2nextboundscale
       :turnpos turnpos :startqscale startqscale :lastnote lastnote)
      (phrase-arch-apply quant)
      ;(rem-all 'ddr)
      ;(rem-all 'dl0)
      )))
|#

(defun phrase-arch 
       (quant &key (shape :x2) (power 2) (amp 1) (phlevel 7)
              (next 1) (2next 1) (turn 2) (last 1) (acc 1))
  (if (not (check-for-phrase-marks))
    (print-ll "Phrase-arch : no phrase marks - skipping rule")
    (progn 
      (phrase-arch-mark-ddr 
       (abs quant) phlevel :shape shape :power power :ampscale amp 
       :nextboundscale next :2nextboundscale 2next
       :turnpos turn :startqscale acc :lastnote last)
      (phrase-arch-apply quant)
      (rem-all 'ddr)
      (rem-all 'dl0)
      )))

(defun phrase-arch-matching (quant &key (power 2) (boundlevel 7) (nextboundscale 1)
                                       (2nextboundscale 1) (turnpos 2) (lastnote 1) (startqscale 1))
      (phrase-arch-mark-ddr-matching (abs quant) boundlevel startqscale nextboundscale
                                         2nextboundscale turnpos power lastnote)
      (phrase-arch-apply-matching quant)
      )


(defun check-for-phrase-marks ()
 (let ((found nil))
  (block loop
  (each-note-if
    (this 'phrase-start)
    (then
      (setq found t)
      (return-from loop) )))
  found))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;            recent stuff
;;;

(defun phrase-arch-mark-ddr 
       (quant boundlevel &key shape power ampscale
              nextboundscale 2nextboundscale turnpos startqscale lastnote)
  (declare (ignore shape))
  (each-note-if
    (this *boundary-start-name*)
    (member boundlevel (this *boundary-start-name*))
    (then
      ;(print-ll "phrase start " (this *boundary-start-name*))
      (let ((peakval-start-tempo (+ (* quant 0.1 startqscale)))  ;quant scaling parameters
            (peakval-end-tempo (+ (* quant 0.2)))
            (peakval-start-l0 (* 0.5 quant ampscale startqscale))
            (peakval-end-l0 (* 1 quant ampscale))
            )
        (let ((istart *i*)
              (iend (i?next-boundlevel *i* *boundary-end-name* boundlevel))
              ipeak
              drdisppeak )

          (cond           ;extend the peakval for higher level endings
           ((member (- boundlevel 2) (iget iend *boundary-end-name*))
            (setq peakval-end-tempo (* peakval-end-tempo 2nextboundscale))
            (setq peakval-end-l0 (* peakval-end-l0 2nextboundscale)) )
           ((member (- boundlevel 1) (iget iend *boundary-end-name*))
            (setq peakval-end-tempo (* peakval-end-tempo nextboundscale))
            (setq peakval-end-l0 (* peakval-end-l0 nextboundscale)) ))

          (cond    ;set ipeak corresponding to the turnpos
           ((integerp turnpos) ;index tone
            (setq ipeak (+ istart turnpos))
            (setq drdisppeak 0.0))
           ((floatp turnpos)
            (multiple-value-setq (ipeak drdisppeak) (i?phrase-rel-length istart iend turnpos)) ))

          (iset-ramp-x2-decimal-last istart ipeak 0.0 drdisppeak peakval-start-tempo 0.0 'ddr power)
          (iset-ramp-x2-decimal-last ipeak iend drdisppeak 0.0 0.0 peakval-end-tempo 'ddr power)
          (when (/= ampscale 0)
            (iset-ramp-x2-decimal-last istart ipeak 0.0 drdisppeak peakval-start-l0 0.0 'dl0 power)
            (iset-ramp-x2-decimal-last ipeak iend drdisppeak 0.0 0.0 peakval-end-l0 'dl0 power) )
          (when (/= lastnote 1)
            (iset iend 'ddr (* (iget iend 'ddr) lastnote)) )
          
          ;(setq *i* iend)
          )))))

;stripped version for rule matching with the parameters independent of quant
;all parameters in percent deviation/100
(defun phrase-arch-mark-ddr-matching
       (quant boundlevel startqscale nextboundscale 2nextboundscale turnpos power lastnote)
  (each-note-if
    (this 'phrase-start)
    (member boundlevel (this 'phrase-start))
    (then
      (let ((peakval-start-tempo startqscale)  ;quant scaling parameters
            (peakval-end-tempo quant)
            )
        (let ((istart *i*)
              (iend (i?next-boundlevel *i* 'phrase-end boundlevel))
              ipeak
              drdisppeak )
          (cond           ;extend the peakval for higher level endings
           ((member (- boundlevel 2) (iget iend 'phrase-end))
            (setq peakval-end-tempo 2nextboundscale)
            )
           ((member (- boundlevel 1) (iget iend 'phrase-end))
            (setq peakval-end-tempo nextboundscale)
            ))
          (multiple-value-setq (ipeak drdisppeak) (i?phrase-rel-length istart iend turnpos))
          (iset-ramp-x2-decimal-last istart ipeak 0.0 drdisppeak peakval-start-tempo 0.0 'ddr power)
          (iset-ramp-x2-decimal-last ipeak iend drdisppeak 0.0 0.0 peakval-end-tempo 'ddr power)
          (iset iend 'ddr lastnote)
          )))))

#|
(defun phrase-arch-apply ()
  (each-note-if
    (this 'ddr)
    (set-this 'dr (* (this 'dr) (+ 1. (this 'ddr)))))
  (each-note-if
    (this 'dl0)
    (not (this 'rest))
    (add-this-l0 (- (this 'dl0))))
  )
;with option to scale the change of duration on the last note on each phrase
;from full (1) to any other factor
(defun phrase-arch-apply (quant boundlevel &key lastnote)
  (when (minusp quant)
    (each-note
      (if (this 'ddr)
        (set-this 'ddr (- (this 'ddr))) )
      (if (this 'dl0)
        (set-this 'dl0 (- (this 'dl0))) )
      ))
  (when (not (= lastnote 1))
    (each-note-if
      (this *boundary-end-name*)
      (member boundlevel (this *boundary-end-name*))
      (this 'ddr)
      (then 
        (set-this 'ddr (* (this 'ddr) lastnote)) )))
  (each-note-if
    (this 'ddr)
    (set-this 'dr (* (this 'dr) (+ 1. (this 'ddr)))))
  (each-note-if
    (this 'dl0)
    (not (this 'rest))
    (add-this-l0 (- (this 'dl0))))
  )
|#


(defun phrase-arch-apply (quant)
  (each-note
    (if (this 'ddr)
      (if (minusp quant)
        (set-this 'dr (* (this 'dr) (- 1.0 (this 'ddr))))
        (set-this 'dr (* (this 'dr) (+ 1.0 (this 'ddr)))) ))
    (if (and (this 'dl0)
             (not (this 'rest)) )
      (if (minusp quant)
        (add-this 'sl (this 'dl0))
        (add-this 'sl (- (this 'dl0))) ))
    ))

(defun phrase-arch-apply-matching (quant)
  (each-note
    (if (minusp quant)
      (set-this 'dr (* (this 'dr) (- 1.0 (this 'ddr))))
      (set-this 'dr (* (this 'dr) (+ 1.0 (this 'ddr)))) )))


;; ------------ help functions -------------
 
;returns the number of the note with boundlevel level
;if not found ->nil
(defun i?next-boundlevel (i prop boundlevel)
  (untilexit end
    (incf i)
    (cond ((>= i (length *v*))
           (return-from end nil))
          ((and (iget i prop)
                (member boundlevel (iget i prop)) )
           (return-from end i))
           )))
(defun i?prev-boundlevel (i prop boundlevel)
  (untilexit end
    (decf i)
    (cond ((<= i 0)
           (return-from end nil))
          ((and (iget i prop)
                (member boundlevel (iget i prop)) )
           (return-from end i))
           )))

#| for testing
(defun foo (boundlevel)
  (each-note-if
    (this *boundary-start-name*)
    (member boundlevel (this *boundary-start-name*))
    (then
    (if *rule-debug-info* (print-ll *i* "   " (this 'n)))
    (if *rule-debug-info* (print (i?next-boundlevel *i* *boundary-end-name* boundlevel)))
)))
|#

(defun iramp-sqrt2-new-decimal (i-from i-to sval eval prop)
  (let ((etime (drsum i-from i-to)))
    (loop for i from i-from to (1- i-to) do
         (let ((time (drsum i-from i)))
           (ifn (iget i 'rest)
                (iset i  prop
                      (+ sval
                         (* (- eval sval)
                            (sqrt (/ time etime)) ))))))))

(defun iramp-sqrt3-new-decimal (i-from i-to sval eval prop)
  (let ((etime (drsum i-from i-to)))
       (loop for i from (1- i-to) downto i-from do
        (let ((time (drsum i i-to)))
         (ifn (iget i 'rest)
           (iset i  prop
               (+ eval
                  (* (- sval eval)
                     (sqrt (/ time etime)) ))))))))

(defun iset-ramp-x2-decimal (i-from i-to sval eval prop)
  (let ((etime (drsum i-from i-to)))
    (cond
     ((<= sval eval)
       (loop for i from i-from to (1- i-to) do
        (let ((time (drsum i-from i)))
           (iset i  prop
               (+ sval
                  (* (- eval sval)
                     (* (/ time etime)(/ time etime)) ))))))
     ((> sval eval)
       (loop for i from i-from to (1- i-to) do
        (let ((time (drsum i i-to)))
           (iset i  prop
               (+ eval
                  (* (- sval eval)
                     (* (/ time etime)(/ time etime)) ))))))
     )))

(defun iset-ramp-x2-decimal-last (i-from i-to sval eval prop &key (power 2))
  (let ((etime (drsum i-from i-to)))
    (if (not (> etime 0.0))
      (warn "the segment does not have a positive duration")
      (cond
       ((<= sval eval)
        (loop for i from i-from to i-to do
             (let ((time (drsum i-from i)))
               (iset i  prop
                     (+ sval
                        (* (- eval sval)
                           (ramp-fn-x2-up (/ time etime) power) ))))))
       ((> sval eval)
        (loop for i from i-from to i-to do
             (let ((time (drsum i-from i)))
               (iset i  prop
                     (+ eval
                        (* (- sval eval)
                           (ramp-fn-x2-down (/ time etime) power) ))))))
       ))))

;;sdisp marks the starting point in ms before the note i-from, should be a negative
;; duration
;;edisp marks the end point in ms after the onset of the last note i-to
;;dr changed to ndr for time
(defun iset-ramp-x2-decimal-last (i-from i-to sdisp edisp sval eval prop power)
  (let ((initdur 0.0))
    (cond ((> sdisp 0)
           (setq initdur (- (iget i-from 'ndr) sdisp))
           (incf i-from 1) ))
    (let ((etime (+ (ndrsum i-from i-to) initdur edisp)))
      (if (not (> etime 0.0))
        () ;(warn "the segment does not have a positive duration")
        (cond
         ((<= sval eval)
          (let ((time initdur))
            (loop for i from i-from to i-to do
                 (iset i  prop
                       (+ sval
                          (* (- eval sval)
                             (ramp-fn-x2-up (/ time etime) power) )))
                 (incf time (iget i 'ndr)) )))
         ((> sval eval)
          (let ((time initdur))
            (loop for i from i-from to i-to do
                 (iset i  prop
                       (+ eval
                          (* (- sval eval)
                             (ramp-fn-x2-down (/ time etime) power) )))
                 (incf time (iget i 'ndr)) )))
         )))))

;applied for 0 <= x <= 1
(defun ramp-fn-x2-up (x power)
  (expt x power))
(defun ramp-fn-x2-down (x power)
  (abs (expt (- x 1) power)))

#|
(defun foo ()
  (each-note-if (first?) (iset-ramp-x2-decimal-last 0 0 50 0 'da0 :power 2.5))
  ;(each-note-if (first?) (iset-ramp-x2-decimal-last 7 10 0 50 'da0 :power 2.5))
  )
|#

;the total nominal duration from note istart to iend
;including iend
(defun ndrtot-ii (istart iend)
  (let ((drsum 0))
    (loop for i from istart to iend do
         (setq drsum (+ drsum (iget i 'ndr))) )
  drsum))

;(defun foo () (each-note-if (first?)(then (print (drtot-ii 0 3)))))


;get the note number for the position where the
;nominal time is 'factor times the total nominal duration
#|
(defun i?phrase-rel-length (istart iend factor)
  (if (or (minusp factor) (>= factor 1))
    (error "factor must be positive below 1 : ~A" factor) )
  (let ((ndrtarget (* factor (ndrtot-ii istart iend)))
        (ndrsum 0) )
    (loop for i from istart to iend do
         (setq ndrsum (+ ndrsum (iget i 'ndr)))
         (if (> ndrsum ndrtarget)
           (return i) ))))
|#

;;;ndrsum used instead of ndrtot-ii: the end is moved to the onset of the last note
;;;the remaining duration after picking the right note is returned as the second value
(defun i?phrase-rel-length (istart iend factor)
  (cond ((minusp factor)
         ;(warn "factor is negative : ~A" factor)
         (values istart 0.0))
        ((> factor 1)
         ;(warn "factor > 1 : ~A" factor)
         (values iend 0.0))
        (t
        (let ((ndrtarget (* factor (ndrsum istart iend)))
              (ndrtime 0.0) )
          (loop for i from istart to iend do
               (incf ndrtime (iget i 'ndr))
               (if (> ndrtime ndrtarget)
                 (return (values i (- (iget i 'ndr) (- ndrtime ndrtarget)))) ))))
        ))

#|
(defun foo (factor) 
  (each-note-if (first?)
                (then (print (multiple-value-list (i?phrase-rel-length 0 5 factor))))))
|#




    


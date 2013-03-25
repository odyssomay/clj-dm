
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

;;9202 cl /af
;;971117/af converted to DM 2
;;20010402/af general normalized functions for the shapes allowed
;;20010414/af negative value for 'turn gives position from the last onset backwards in ms

(in-package :dm)


;;local vars to be used only within the rule
(defvar *boundary-start-name*)
(defvar *boundary-end-name*)
(setq *boundary-start-name* 'phrase-start)
(setq *boundary-end-name* 'phrase-end)

;-------------baroque phrasing-----------------------------------------
;;an attempt to set parameters for the phrase-arch rule so
;;that only a ritardando is performed in the end of each, presumably long phrase
(defun phrase-ritardando 
    (quant &key (shape :x2) (power 3.5) (amp 2) (phlevel 4)
           (next 1.5) (2next 2) (turn -2500) (last 1) (acc 0)
           (accfn 'power-fn-acc) (decfn 'power-fn-dec) )
  (phrase-arch quant :shape shape :power power :amp amp :phlevel phlevel
               :next next :2next 2next
               :turn turn :acc acc :last last
               :accfn accfn :decfn decfn)
  )

;-------------main function-----------------------------------------

(defun phrase-arch 
       (quant &key (shape :x2) (power 2) (amp 1) (dur 1) (phlevel 7)
              (next 1) (2next 1) (turn 2) (last 1) (acc 1)
              (accfn 'power-fn-acc) (decfn 'power-fn-dec) )
  (if (not (check-for-phrase-marks))
    (print-ll "Phrase-arch : no phrase marks - skipping rule")
    (progn 
      ;(print-ll " shape" shape " power " power  " amp " amp " phlevel "  phlevel " next " next "turn " turn last acc)

      (phrase-arch-mark-ddr 
       (abs quant) phlevel :shape shape :power power :ampscale amp 
       :nextboundscale next :2nextboundscale 2next
       :turnpos turn :startqscale acc :lastnote last
       :accfn accfn :decfn decfn)
      (phrase-arch-apply quant :dur dur)
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

;;;---------------------- mark ddr --------------------------

(defun phrase-arch-mark-ddr 
       (quant boundlevel &key shape power ampscale
              nextboundscale 2nextboundscale turnpos startqscale lastnote
              accfn decfn)
  (declare (ignore shape))
  (each-note-if
    (this *boundary-start-name*)
    (member boundlevel (this *boundary-start-name*))
    (then
      ;(print-ll "phrase start " (this *boundary-start-name*) " i " *i*)
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
           ((minusp turnpos)  ;from end in ms
            (multiple-value-setq (ipeak drdisppeak)
              (i?phrase-time-before-end istart iend (- turnpos)) ))
           ((integerp turnpos) ;index tone
            (setq ipeak (+ istart turnpos))
            (setq drdisppeak 0.0))
           ((floatp turnpos)
            (multiple-value-setq (ipeak drdisppeak) (i?phrase-rel-length istart iend turnpos)) ))

          (iset-ramp-x2-decimal-last istart ipeak 0.0 drdisppeak peakval-start-tempo 0.0 'ddr power accfn decfn)
          (iset-ramp-x2-decimal-last ipeak iend drdisppeak 0.0 0.0 peakval-end-tempo 'ddr power accfn decfn)
          (when (/= ampscale 0)
            (iset-ramp-x2-decimal-last istart ipeak 0.0 drdisppeak peakval-start-l0 0.0 'dl0 power accfn decfn)
            (iset-ramp-x2-decimal-last ipeak iend drdisppeak 0.0 0.0 peakval-end-l0 'dl0 power accfn decfn) )
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


;;----------- apply -------------------------

(defun phrase-arch-apply (quant &key dur)
  ;(print-ll " dur " dur)
  (each-note
    (if (this 'ddr)
      (if (minusp quant)
        (set-this 'dr (* (this 'dr) (- 1.0 (* dur (this 'ddr)))))
        (set-this 'dr (* (this 'dr) (+ 1.0 (* dur (this 'ddr))))) ))
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
(defun i?next-boundlevel (i-start prop boundlevel)
  (let ((i i-start))
  (untilexit end
             (incf i)
             (cond ((>= i (length *v*))
                    (error "no next boundlevel=~A i-start=~A  prop=~A segment=~A "
                      boundlevel i-start  prop (nth i-start  *v*))     
                    (return-from end nil))
                   ((and (iget i prop)
                         (member boundlevel (iget i prop)) )
                    (return-from end i))
                   ))))
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

#|
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
|#

;;sdisp marks the starting point in ms before the note i-from, should be a negative
;; duration
;;edisp marks the end point in ms after the onset of the last note i-to
;;dr changed to ndr for time
#|
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
|#
(defun iset-ramp-x2-decimal-last (i-from i-to sdisp edisp sval eval prop power accfn decfn)
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
                             (funcall decfn (/ time etime) power) )))
                 (incf time (iget i 'ndr)) )))
         ((> sval eval)
          (let ((time initdur))
            (loop for i from i-from to i-to do
                 (iset i  prop
                       (+ eval
                          (* (- sval eval)
                             (funcall accfn (/ time etime) power) )))
                 (incf time (iget i 'ndr)) )))
         )))))

#| testing
(defun foo ()
  (each-note-if (first?) (iset-ramp-x2-decimal-last 0 10 0.0 0.0 50.0 0.0 'da0 2.5 'power-fn-acc 'power-fn-dec))
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

;;;return the index of the note that is 'ndrtarget before iend
;;;the remaining duration after picking the right note is returned as the second value
#|
(defun i?phrase-time-before-end (istart iend ndrtarget)
        (let ((i iend) (ndrtot 0))
          (while (< ndrtot ndrtarget)
            (decf i)
            (incf ndrtot (iget i 'ndr)) )
          (if (>= i istart)
              (values i (- ndrtot ndrtarget))
              (values istart 0)   ;if before start
            )))
|#
;fixed if phrase shorter than ndrtarget
(defun i?phrase-time-before-end (istart iend ndrtarget)
        (let ((i iend) (ndrtot 0))
          (while (and (< ndrtot ndrtarget) (> i 0))
            (decf i)
            (incf ndrtot (iget i 'ndr)) )
          (if (>= i istart)
              (values i (- ndrtot ndrtarget))
              (values istart 0)   ;if before start
          )))

#|
(defun foo (factor) 
  (each-note-if (first?)
                (then (print (multiple-value-list (i?phrase-rel-length 0 5 factor))))))
|#


;;----------- phrase shape functions --------------------------


;;all functions are normalized with 0 <= x <= 1
;; the deceleration function should have y from 0 to 1
;; the accerelation function should have y from 1 to 0



;;----power function
;;the default phrasing
(defun power-fn-dec (x power)
  (expt x power))
(defun power-fn-acc (x power)
  (abs (expt (- x 1) power)))

;;----runners deceleration function
;;from the model in Friberg and Sundberg (1999)
;;one over formula (2) with vend = 0.5
;;power=q=3 gives the average runners deceleration
;;power=q=2 gives linear tempo as a function of time (Todd, 1992, 1995)
;; these will hold only when vend is 0.5 that is dIOI is 100%
(defun runrit-fn-dec (x power)
  (1- (/ 1.0 (expt (+ 1.0 (* (- (expt 0.5 power) 1.0) x)) (/ 1.0 power)))) )

(defun runrit-fn-acc (x power)
  (runrit-fn-dec (- 1.0 x) power) )

;;---- hand gesture function
;;from Flash and Hogan 85
;;speed factor 2
;; power parameter not used
(defun hand-gesture-fn-dec (x power)
  (GET-VALUE-AT *hand-gesture-shape-dec* x))
(defun hand-gesture-fn-acc (x power)
  (GET-VALUE-AT *hand-gesture-shape-acc* x))


(defvar *hand-gesture-shape-dec*)
(setq *hand-gesture-shape-dec* (make-instance 'linear-bp-shape))
(insert-break-point-list 
 *hand-gesture-shape-dec* 
 '(
   0	0
   0.045012019	0.000806363
   0.089952042	0.00323127
   0.134748248	0.007292268
   0.17932916 	0.013018917
   0.22362382 	0.020453268
   0.267561962	0.029650559
   0.311074183	0.040680137
   0.35409212 	0.053626639
   0.396548619	0.068591473
   0.438377907	0.085694632
   0.479515771	0.105076918
   0.519899723	0.126902629
   0.55946918 	0.151362823
   0.598165632	0.178679259
   0.635932817	0.209109188
   0.672716894	0.242951152
   0.708466613	0.280552075
   0.743133495	0.322315917
   0.776671994	0.36871434
   0.809039682	0.42029987
   0.840197413	0.477722296
   0.870109498	0.541749179
   0.89874388 	0.613291741
   0.926072307	0.693437764
   0.9520705  	0.78349378
   0.976718333	0.885039667
   1	1
   ))

(defvar *hand-gesture-shape-acc*)
(setq *hand-gesture-shape-acc* (make-instance 'linear-bp-shape))
(insert-break-point-list 
 *hand-gesture-shape-acc* 
 '(
   0	1
   0.023281667	0.885039667
   0.0479295  	0.78349378
   0.073927693	0.693437764
   0.10125612 	0.613291741
   0.129890502	0.541749179
   0.159802587	0.477722296
   0.190960318	0.42029987
   0.223328006	0.36871434
   0.256866505	0.322315917
   0.291533387	0.280552075
   0.327283106	0.242951152
   0.364067183	0.209109188
   0.401834368	0.178679259
   0.44053082 	0.151362823
   0.480100277	0.126902629
   0.520484229	0.105076918
   0.561622093	0.085694632
   0.603451381	0.068591473
   0.64590788 	0.053626639
   0.688925817	0.040680137
   0.732438038	0.029650559
   0.77637618 	0.020453268
   0.82067084 	0.013018917
   0.865251752	0.007292268
   0.910047958	0.00323127
   0.954987981	0.000806363
   1	0
   
   ))

;;----------------end----------------------------




    


;;; old stuff in the phrase parabola rule
;;; keep it - AF

(in-package "DM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;        previous stuff
;;; 
#|
(defun phrase-arch-x2-level6-upper (quant)
  (phrase-arch 
   quant :shape :x2 :power 2 :boundlevel 6 :nextboundscale 2 :2nextboundscale 3))

(defun phrase-arch-mark-min-max (boundlevel)
 (each-note-if
    (this *boundary-start-name*)
    (member boundlevel (this *boundary-start-name*))
    (then
       ;(print *i*)
       (let ((istart *i*)
             (iend (i?next-boundlevel *i* *boundary-end-name* boundlevel))
             (fmin 128)(fmax 0)(imax)(imin) )
         ;(print-ll istart "  " iend)
         (loop for i from istart to iend do
            (if (> (iget i 'f0) fmax)
                (progn (setq fmax (iget i 'f0))
                       (setq imax i)))
            (if (<= (iget i 'f0) fmin)
                (progn (setq fmin (iget i 'f0))
                       (setq imin i)) ))

          ;tone rep. the last two?
         (if (and (= (1+ imax) iend)
                  (= (iget imax 'f0) (iget iend 'f0)))
             (setq imax iend) )
         (if (and (= (1+ imin) iend)
                  (= (iget imin 'f0) (iget iend 'f0)))
             (setq imin iend) )

         ;if max second last and more than 3 notes, move to last
         (if (and (= (1+ imax) iend)
                  (> (- iend istart) 3))
             (setq imax iend) )
         (if (and (= (1+ imin) iend)
                  (> (- iend istart) 3))
             (setq imin iend) )
         
         ;if max second note and more than 3 notes, move to first
         (if (and (= (1- imax) istart)
                  (> (- iend istart) 3))
             (setq imax istart) )
         (if (and (= (1- imin) istart)
                  (> (- iend istart) 3))
             (setq imin istart) )

         (iset imax 'f0-curve 'max)
         (iset imin 'f0-curve 'min)
         (setq *i* iend)
           ))))

(defun phrase-arch-mark-ddr (quant boundlevel &key shape power)
  (let ((curve (or shape :sqrt2))) ; :cos :lin :sqrt :sqrt2
    (if *rule-debug-info* (print-ll "phrase-arch-mark-ddr : shape = " curve))
    (each-note-if
    (this *boundary-start-name*)
    (member boundlevel (this *boundary-start-name*))
      (then
        (let ((peakval-tempo (- (* quant 0.2)))  ;quant scaling parameters
              (peakval-l0 (* quant 4))
              (down-slope-scaling 0.7) )
          ;(print *i*)
          (let ((istart *i*)
                (iend (i?next-boundlevel *i* *boundary-end-name* boundlevel))
                (ipeak (i?next *i* 'f0-curve))
                (ipeak2))
            (if (and (not (member (1- boundlevel) (this *boundary-start-name*)))  ;;???
                     (not (first?))
                     (not (prev 'rest)) )
              (setq istart (1- istart)) )
            (if *rule-debug-info* (print-ll istart "  " iend "  " ipeak))
            
            ;scale quant rel duration
            (let ((drtot 0.))
              (loop for i from istart to iend do
                   (setq drtot (+ drtot (this 'dr))) )
              (setq peakval-tempo (* peakval-tempo (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0)))
              (setq peakval-l0 (* peakval-l0 (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0))) )
            
            ;scale quant rel downslope
            (if (< (iget iend 'f0) (iget istart 'f0))
              (progn
                (if *rule-debug-info* (print "down"))
                (setq peakval-tempo (* peakval-tempo down-slope-scaling))
                (setq peakval-l0 (* peakval-l0 down-slope-scaling)) ))
            
            ;put ipeak in the middle or first right of middle if no min/max
            (if (or (not ipeak)(>= ipeak iend))
              (setq ipeak (+ istart (round (+ -0.25 (/ (float (- (1+ iend) istart)) 2.))))) )

            ;take last peak if several
            (if (and (i?next ipeak 'f0-curve)
                     (< (i?next ipeak 'f0-curve) iend))
              (setq ipeak (i?next ipeak 'f0-curve)) )

            ;if peak is close to iend move one note to the middle
            (if (and (>= (- iend istart) 6)
                     (<= (- iend ipeak) 2) )
              (decf ipeak))

            ;always in the middle
            ;(setq ipeak (+ istart (round (+ -0.25 (/ (float (- (1+ iend) istart)) 2.)))))
            ;(setq ipeak2 ipeak)
            
            ;second tone
            (setq ipeak (+ istart 1))
            (setq ipeak2 ipeak)
            
            ;put the peak for the end of the acc. at the end of the next lower level
            ;(print-ll "start,end,nextbound " istart " " iend " " (i?next istart *boundary-end-name*))
            ;(if (and (member (1+ boundlevel) 
            ;                 (iget (i?next istart *boundary-end-name*) *boundary-end-name*))
            ;         (< (i?next istart *boundary-end-name*) iend))
            ;  (setq ipeak (i?next istart *boundary-end-name*)) )
            
            ;put the peak for the start of the rit. at the beginning of the first previous lower level
            ;start
            ;(if (and (member (1+ boundlevel) 
            ;                 (iget (i?prev iend *boundary-start-name*) *boundary-start-name*))
            ;         (> (i?prev iend *boundary-start-name*) istart))
            ;  (setq ipeak2 (1- (i?prev iend *boundary-start-name*))) )

            (if *rule-debug-info* (print-ll "start,peak,peak2,end " istart " " ipeak " " ipeak2 " " iend " " ))
            (setq peakval-l0 (- peakval-l0))
            (let ((peakval-tempo-end (* (- peakval-tempo) 0.5)) ;scaling of last segment
                  (peakval-l0-end (* (- peakval-l0) 0.5)) )
              (cond 
               ((eq :cos curve)
                ;(print "cos interpolation")
                (iramp-cos-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
                (iramp-cos-new-decimal ipeak2 iend peakval-tempo peakval-tempo-end 'ddr)
                (iramp-cos-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
                (iramp-cos-new-decimal ipeak2 iend peakval-l0 peakval-l0-end 'dl0))
               ((eq :lin curve)
                ;(print "lin interpolation")
                (iramp-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
                (iramp-new-decimal ipeak2 iend peakval-tempo peakval-tempo-end 'ddr)
                (iramp-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
                (iramp-new-decimal ipeak2 iend peakval-l0 peakval-l0-end 'dl0))
               ((eq :x2 curve)
                ;(print "x2 interpolation")
                (iset-ramp-x2-decimal-last istart ipeak 0.0 peakval-tempo 'ddr :power power)
                (iset-ramp-x2-decimal-last ipeak2 iend peakval-tempo peakval-tempo-end 'ddr :power power)
                (iset-ramp-x2-decimal-last istart ipeak 0.0 peakval-l0 'dl0 :power power)
                (iset-ramp-x2-decimal-last ipeak2 iend peakval-l0 peakval-l0-end 'dl0 :power power)))
              (if (> ipeak2 ipeak)
                (loop for i from ipeak to ipeak2 do
                     (iset i 'ddr peakval-tempo)
                     (iset i 'dl0 peakval-l0)
                     ))
              (setq *i* iend)
              )))))))

;Hagegård model
;flytta frasstart till slutet av föregående fras
;nextboundscale scale peakval of the end of the frase differently if the
;next upper level ends there too. = 1 means do nothing

(defun phrase-arch-mark-ddr 
       (quant boundlevel &key shape power ampscale nextboundscale 2nextboundscale turnpos startqscale)
  (if *rule-debug-info* 
    (print-ll "phrase-arch-mark-ddr : shape = " shape 
              " nextboundscale = " nextboundscale
              " 2nextboundscale = " 2nextboundscale
              " turnpos = " turnpos))

  (each-note-if
    (this *boundary-start-name*)
    (member boundlevel (this *boundary-start-name*))
    (then
      (let ((peakval-start-tempo (+ (* quant 0.1 startqscale)))  ;quant scaling parameters
            (peakval-end-tempo (+ (* quant 0.2)))
            (peakval-start-l0 (* 0.5 quant ampscale startqscale))
            (peakval-end-l0 (* 1 quant ampscale))
            ;(down-slope-scaling 0.7)
            )

        (let ((istart *i*)
              (iprevend (i?prev-boundlevel *i* *boundary-end-name* boundlevel))
              (iend (i?next-boundlevel *i* *boundary-end-name* boundlevel))
              (ipeak ) ;(i?next *i* 'f0-curve)
              (ipeak2) )
          (cond           ;extend the peakval for higher level endings
           ((member (- boundlevel 2) (iget iend *boundary-end-name*))
            (setq peakval-end-tempo (* peakval-end-tempo 2nextboundscale))
            (setq peakval-end-l0 (* peakval-end-l0 2nextboundscale)) )
           ((member (- boundlevel 1) (iget iend *boundary-end-name*))
            (setq peakval-end-tempo (* peakval-end-tempo nextboundscale))
            (setq peakval-end-l0 (* peakval-end-l0 nextboundscale)) ))
          (if *rule-debug-info* (print-ll "peakval-end-tempo = " peakval-end-tempo
                                          "  peakval-end-l0 = "peakval-end-l0 ))
          ; (if (not iprevend) 
          ;   (setq iprevend istart)
          ;   (setq peakval-start-tempo peakval-end-tempo) )
          (setq iprevend istart)
          ; (if (and (not (member (1- boundlevel) (this *boundary-start-name*)))  ;;???
          ;          (not (first?))
          ;          (not (prev 'rest)) )
          ;   (setq istart (1- istart)) )
          (if *rule-debug-info* (print-ll istart "  " iprevend "  " iend "  " ipeak))
          
          ;scale quant rel duration
          ; (let ((drtot 0.))
          ;   (for (i istart 1 iend)
          ;        (setq drtot (+ drtot (this 'dr))) )
          ;   (setq peakval-tempo (* peakval-tempo (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0)))
          ;   (setq peakval-l0 (* peakval-l0 (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0))) )
          
          ;scale quant rel downslope
          ; (if (< (iget iend 'f0) (iget istart 'f0))
          ;   (progn
          ;     (print "down")
          ;     (setq peakval-tempo (* peakval-tempo down-slope-scaling))
          ;     (setq peakval-l0 (* peakval-l0 down-slope-scaling)) ))
          
          ;put ipeak in the middle or first right of middle if no min/max
          ;(if (or (not ipeak)(>= ipeak iend))
          ;  (setq ipeak (+ istart (round (+ -0.25 (/ (float (- (1+ iend) istart)) 2.))))) )
          
          ;take last peak if several
          ;(if (and (i?next ipeak 'f0-curve)
          ;         (< (i?next ipeak 'f0-curve) iend))
          ;  (setq ipeak (i?next ipeak 'f0-curve)) )
          
          ;if peak is close to iend move one note to the middle
          ;(if (and (>= (- iend istart) 6)
          ;         (<= (- iend ipeak) 2) )
          ;  (decf ipeak))
          
          (cond 
           ((eq turnpos :middle) ;always in the middle          
            (setq ipeak (i?phrase-rel-length istart iend 0.5))
            (setq ipeak2 ipeak) )
           ((integerp turnpos) ;index tone
            (setq ipeak (+ istart turnpos))
            (setq ipeak2 ipeak) )
           ((floatp turnpos)
            (setq ipeak (i?phrase-rel-length istart iend turnpos))
            (setq ipeak2 ipeak) ))
          
          ;put the peak for the end of the acc. at the end of the next lower level
          ;(print-ll "start,end,nextbound " istart " " iend " " (i?next istart *boundary-end-name*))
          ;(if (and (member (1+ boundlevel) 
          ;                 (iget (i?next istart *boundary-end-name*) *boundary-end-name*))
          ;         (< (i?next istart *boundary-end-name*) iend))
          ;  (setq ipeak (i?next istart *boundary-end-name*)) )
          
          ;put the peak for the start of the rit. at the beginning of the first previous lower level
          ;start
          ;(if (and (member (1+ boundlevel) 
          ;                 (iget (i?prev iend *boundary-start-name*) *boundary-start-name*))
          ;         (> (i?prev iend *boundary-start-name*) istart))
          ;  (setq ipeak2 (1- (i?prev iend *boundary-start-name*))) )
          
          (if *rule-debug-info* (print-ll "start,peak,peak2,end " istart " " ipeak " " ipeak2 " " iend " " ))
          ;(setq peakval-l0 (- peakval-l0))
          (cond 
           ((eq :x2 shape)
            ;(print "x2 interpolation")
            (iset-ramp-x2-decimal-last iprevend ipeak peakval-start-tempo 0.0 'ddr :power power)
            (iset-ramp-x2-decimal-last ipeak2 iend 0.0 peakval-end-tempo 'ddr :power power)
            (when (/= ampscale 0)
              (iset-ramp-x2-decimal-last iprevend ipeak peakval-start-l0 0.0 'dl0 :power power)
              (iset-ramp-x2-decimal-last ipeak2 iend 0.0 peakval-end-l0 'dl0 :power power) )))
          ; (if (> ipeak2 ipeak)
          ;   (for (i ipeak 1 ipeak2)
          ;        (iset i 'ddr peakval-tempo)
          ;        (iset i 'dl0 peakval-l0)
          ;        ))
          ;(setq *i* iend)
          )))))
|#    

#|
;;-------------------------------------------------------------------------------------
;;------------------------ old stuff --------------------------------------------------
;;-------------------------------------------------------------------------------------

(defun phrase-arch-lin (quant)
  (phrase-arch quant :shape :lin))

(defun phrase-arch-cos (quant)
  (phrase-arch quant :shape :cos))

(defun phrase-arch-sqrt (quant)
  (phrase-arch quant :shape :sqrt))

(defun phrase-arch-sqrt2 (quant)
  (phrase-arch quant :shape :sqrt2))

(defun phrase-arch-sqrt3 (quant)
  (phrase-arch quant :shape :sqrt3))

(defun phrase-arch-x2 (quant)
  (phrase-arch quant))

(defun phrase-arch-x3 (quant)
  (phrase-arch quant :shape :x2 :power 3))

(defun phrase-arch-x4 (quant)
  (phrase-arch quant :shape :x2 :power 4))

(defun phrase-arch-x2-level7 (quant)
  (phrase-arch quant :shape :x2 :power 2 :boundlevel 7))
(defun phrase-arch-x3-level7 (quant)
  (phrase-arch quant :shape :x2 :power 3 :boundlevel 7))
(defun phrase-arch-x4-level7 (quant)
  (phrase-arch quant :shape :x2 :power 4 :boundlevel 7))

(defun phrase-arch-x2-level6 (quant)
  (phrase-arch quant :shape :x2 :power 2 :boundlevel 6))
(defun phrase-arch-x3-level6 (quant)
  (phrase-arch quant :shape :x2 :power 3 :boundlevel 6))
(defun phrase-arch-x4-level6 (quant)
  (phrase-arch quant :shape :x2 :power 4 :boundlevel 6))

(defun phrase-arch-x2-level5 (quant)
  (phrase-arch quant :shape :x2 :power 2 :boundlevel 5))
(defun phrase-arch-x3-level5 (quant)
  (phrase-arch quant :shape :x2 :power 3 :boundlevel 5))
(defun phrase-arch-x4-level5 (quant)
  (phrase-arch quant :shape :x2 :power 4 :boundlevel 5))

(defun phrase-arch-x2-level4 (quant)
  (phrase-arch quant :shape :x2 :power 2 :boundlevel 4))
(defun phrase-arch-x3-level4 (quant)
  (phrase-arch quant :shape :x2 :power 3 :boundlevel 4))
(defun phrase-arch-x4-level4 (quant)
  (phrase-arch quant :shape :x2 :power 4 :boundlevel 4))


(defun phrase-arch (quant &key (shape :x2) (power 2) (boundlevel 7))
   ;(pitch-acc-skip-rests)
   (phrase-arch-mark-min-max boundlevel)
   ;(pitch-acc-rem-min-min)
   (phrase-arch-mark-ddr quant boundlevel :shape shape :power power)
   (phrase-arch-apply)
   ;(pitch-acc-mark-rests)
         )



;-------

(defun dr-norm-phrase ()
(setq *phrase-start-name* 'phrase-start)
(setq *phrase-end-name* 'phrase-end)
(dr-norm-motive-aux))

(defun dr-norm-motive ()
(setq *phrase-start-name* 'motive-start)
(setq *phrase-end-name* 'motive-end)
(dr-norm-motive-aux))

(defun dr-norm-motive-aux ()
 (let ((fact))
  (mark-motive-ddr)
  (mark-motive-dr)
  (each-note
    (if (this 'motive-ddr)
        (setq fact (/ (- (this 'motive-dr) (this 'motive-ddr)) (this 'motive-dr))) )
    (set-this 'dr (* fact (this 'dr))) ))
 (rem-all 'motive-ddr)
 (rem-all 'motive-dr)
 )

;mark duration deviation for each bar
(defun mark-motive-ddr ()
  (each-note-if
       (this *phrase-start-name*)
       (i?next *i* *phrase-end-name*)
       (then
         ;(print "hwer2")
         (set-this 'motive-ddr (- (drsum *i* (i?next *i* *phrase-end-name*))
                              (ndrsum *i* (i?next *i* *phrase-end-name*)) 
                              )))))

(defun mark-motive-dr ()
  (each-note-if
       (this *phrase-start-name*)
       (i?next *i* *phrase-end-name*)
       (then
         (set-this 'motive-dr (drsum *i* (i?next *i* *phrase-end-name*)))
         )))


;(defun foo () (pitch-acc-phrase 1))

;--------- pitch acc on groups --------------------------------------

(defun pitch-acc-punct (quant)
   (mark-punctuation)
   (rem-all 'motive)
   (pitch-acc-skip-rests)
   (pitch-acc-mark-f0-mean-punct)
   (pitch-acc-mark-min-max-punct)
   (pitch-acc-mark-ddr-punct quant)
   (pitch-acc-apply)
   (pitch-acc-mark-rests)   )



(defun pitch-acc-mark-f0-mean-punct ()
   (each-note-if
      (or (first?)(prev 'weight))
      (then
        (let ((sum 0.0)(next-i (i?next (1- *i*) 'weight)))
           (if (not next-i) (setq next-i (i?last)))
           (cond 
             ((= *i* next-i)
              ; (set-this 'f0-mean (this 'f0))
              ) ;single note
             (t 
              (loop for i from *i* to next-i do
                 (setq sum (+ sum (iget i 'f0))) )
              (set-this 'f0-mean (/ sum (1+ (- next-i *i*)))) ))
           (if *rule-debug-info* (print-ll sum " *i* " *i* " next-i " next-i " mean " (this 'f0-mean)))
           ))))            
            
(defun pitch-acc-mark-min-max-punct ()
 (each-note-if        ;first note
   (first?)
   (this 'f0-mean)
   (then
     (let ((next-i (i?next *i* 'f0-mean)))
       (cond 
         ((> (this 'f0-mean) (iget next-i 'f0-mean))
          (set-this 'f0-curve 'max) )
         ((< (this 'f0-mean) (iget next-i 'f0-mean))
          (set-this 'f0-curve 'min) )))))
 (each-note-if        ;last note with f0-mean
   (last?)
   (then
     (let ((prev-i (i?prev *i* 'f0-mean)))
       (cond 
         ((> (iget prev-i 'f0-mean) (iget (i?prev prev-i 'f0-mean) 'f0-mean))
          (set-this 'f0-curve 'max) )
         ((< (iget prev-i 'f0-mean) (iget (i?prev prev-i 'f0-mean) 'f0-mean))
          (set-this 'f0-curve 'min) )))))
 (each-note-if
   (not (first?))
   (this 'f0-mean)
   (i?next *i* 'f0-mean)
   (then
     (let 
       ((prev-i (i?prev *i* 'f0-mean))
        (next-i (i?next *i* 'f0-mean)) )
       ;(ifn next-i (setq next-i (i?last)))
       (cond 
         ((and (> (this 'f0-mean) (iget prev-i 'f0-mean))
               (> (this 'f0-mean) (iget next-i 'f0-mean)) )
          (set-this 'f0-curve 'max) )
         ((and (< (this 'f0-mean) (iget prev-i 'f0-mean))
               (< (this 'f0-mean) (iget next-i 'f0-mean)) )
          (set-this 'f0-curve 'min) ))))))

(defun pitch-acc-mark-ddr-punct (quant)
 (let ((upval (* quant -0.07)) ;maximum deviation uphill
       (downval (* quant -0.035)) ;maximum deviation downhill
       (start-i 0)
       (end-i 0)
       (middle-i 0)
       (drsum 0.) )
  (each-note-if
   (i?next (1- *i*) 'f0-curve)
   (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve)
   (then 
     (setq start-i (i?next (1- *i*) 'f0-curve))
     (setq end-i (i?next start-i 'f0-curve))
     (setq drsum (+ (drsum start-i end-i) (iget end-i 'dr)))
     (cond
        ((and (eq (iget start-i 'f0-curve) 'min)        ;upslope
              (eq (iget end-i 'f0-curve) 'max) )
         (setq middle-i (i?drsum start-i (/ drsum 2.)))
         (if (= middle-i start-i)(incf middle-i))
        (if *rule-debug-info* (print-ll "min-max" "start " start-i " middle " middle-i " end " end-i))
         (iramp-mean-new-decimal start-i middle-i 0.0 upval 'ddr)
         (iramp-mean-new-decimal middle-i end-i upval 0.0 'ddr) )
        ((and (eq (iget start-i 'f0-curve) 'max)        ;downslope
              (eq (iget end-i 'f0-curve) 'min) )
         (setq middle-i (i?drsum start-i (/ drsum 2.)))
         (if (= middle-i start-i)(incf middle-i))
        (if *rule-debug-info* (print-ll "max-min" "start " start-i " middle " middle-i " end " end-i))
         (iramp-mean-new-decimal start-i middle-i 0.0 downval 'ddr)
         (iramp-mean-new-decimal middle-i end-i downval 0.0 'ddr) ))
     (setq *i* start-i)            
     ))))
  


;generates faster tempo in uphill and downhill pitchmotion
;work in progress/af

(defun pitch-acc-tot (quant)
  (pitch-acc quant)
  (rem-all 'f0-curve)
  (rem-all 'f0-mean)
  (pitch-acc-punct quant) )


(defun pitch-acc (quant)
  (rem-all 'f0-curve)
  (rem-all 'f0-mean)
   (pitch-acc-skip-rests)
   ;(pitch-acc-mark-f0-mean-5)
   (pitch-acc-mark-min-max)
   (pitch-acc-rem-min-min)
   (pitch-acc-mark-ddr quant)
   (pitch-acc-apply)
   (pitch-acc-mark-rests)   )

;doesn't work with rests
(defun pitch-acc-mark-f0-mean-5 ()
  (each-note-if
    (not (first?))
    (not (last?))
    (not (first+1?))
    (not (last-1?))
    ;(not (this 'rest))
    ;(not (prev 'rest))
    ;(not (next 'rest))
    (then
      (let ((f0-mean 0.0))
         (setq f0-mean
            (+
              (* 0.25 (iget-f0 (- *i* 2)))
              (* 0.75 (iget-f0 (- *i* 1)))
              (* 1.00 (iget-f0    *i*   ))
              (* 0.75 (iget-f0 (+ *i* 1)))
              (* 0.25 (iget-f0 (- *i* 2))) ))
        (setq f0-mean (/ f0-mean 3.))
        (set-this 'f0-mean f0-mean) )))
  (each-note-if
    (or (or (or (first?) (first+1?)) (last?)) (last-1?))
    (then
      (set-this 'f0-mean (this-f0)) ))
   )


(defun pitch-acc-skip-rests ()
  (each-note-if
    (this 'rest)
    (set-this 'f0 (prev-f0)) )
  (each-note-if
    (first?)
    (this 'rest)
    (loop for i from 0 to (1- (i?next-note 0)) do
      (iset i 'f0 (iget-f0 (i?next-note 0))) ))
  )

(defun pitch-acc-mark-rests ()
  (each-note-if
    (this 'rest)
    (rem-this 'f0) ))


(defun pitch-acc-mark-min-max ()
  (each-note-if
    (not (first?))
    (not (last?))
    (not (first+1?))
    (not (last-1?))
  ;  (not (this 'rest))
  ;  (not (prev 'rest))
  ;  (not (next 'rest))
    (let ((f0-mean (this 'f0-mean)))
      (when (and
             (>  f0-mean (iget (- *i* 2) 'f0-mean))
             (>  f0-mean (iget (- *i* 1) 'f0-mean))
             (>= f0-mean (iget (+ *i* 1) 'f0-mean))
             (>  f0-mean (iget (+ *i* 2) 'f0-mean)) )
         (set-this 'f0-curve 'max) )
      (when (and
             (<  f0-mean (iget (- *i* 2) 'f0-mean))
             (<  f0-mean (iget (- *i* 1) 'f0-mean))
             (<= f0-mean (iget (+ *i* 1) 'f0-mean))
             (<  f0-mean (iget (+ *i* 2) 'f0-mean)) )
         (set-this 'f0-curve 'min) )))
   )
(defun pitch-acc-mark-min-max ()
  (each-note-if
    (first?)
    (let ((f0 (this 'f0)))
      (if (and
             (>= f0 (next 'f0))
             (>  f0 (next2 'f0)) )
         (set-this 'f0-curve 'max) )
      (if (and
             (<= f0 (next 'f0))
             (<  f0 (next2 'f0)) )
         (set-this 'f0-curve 'min) )
      (exit-voice) ))
  (each-note-if
    (first+1?)
    (let ((f0 (this 'f0)))
      (if (and
             (>  f0 (prev 'f0))
             (>= f0 (next 'f0))
             (>  f0 (next2 'f0)) )
         (set-this 'f0-curve 'max) )
      (if (and
           (<= f0 (next 'f0))
           (<  f0 (next2 'f0)))
        (set-this 'f0-curve 'min) )
      (exit-voice) ))
  (each-note-if
    (last-1?)
    (let ((f0 (this 'f0)))
      (if (and
             (>  f0 (prev2 'f0))
             (>  f0 (prev 'f0))
             (>= f0 (next 'f0)) )
         (set-this 'f0-curve 'max) )
      (if (and
             (<  f0 (prev2 'f0))
             (<  f0 (prev 'f0))
             (>= f0 (next 'f0)) )
         (set-this 'f0-curve 'min) )
      ))
  (each-note-if
    (last?)
    (let ((f0 (this 'f0)))
      (if (and
             (>  f0 (prev2 'f0))
             (>  f0 (prev 'f0)) )
         (set-this 'f0-curve 'max) )
      (if (and
             (<  f0 (prev2 'f0))
             (<  f0 (prev 'f0)) )
         (set-this 'f0-curve 'min) )
      ))
  (each-note-if
    (not (first?))
    (not (last?))
    (not (first+1?))
    (not (last-1?))
  ;  (not (this 'rest))
  ;  (not (prev 'rest))
  ;  (not (next 'rest))
    (let ((f0 (this 'f0)))
      (when (and
             (>  f0 (prev2 'f0))
             (>  f0 (prev 'f0))
             (>= f0 (next 'f0))
             (>  f0 (next2 'f0)) )
         (set-this 'f0-curve 'max) )
      (when (and
             (<  f0 (prev2 'f0))
             (<  f0 (prev 'f0))
             (<= f0 (next 'f0))
             (<  f0 (next2 'f0)) )
         (set-this 'f0-curve 'min) )))
   )


;remove one of two conseq. min or two conseq. max 
(defun pitch-acc-rem-min-min ()
  (each-note-if   ;rem highest of two min
   (i?next (1- *i*) 'f0-curve)
   (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve)
   (eq 'min (iget (i?next (1- *i*) 'f0-curve) 'f0-curve))
   (eq 'min (iget (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve) 'f0-curve))
   (then
    (let 
     ((i1 (i?next (1- *i*) 'f0-curve)) 
      (i2 (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve)))
     (if (< (iget-f0 i1)(iget-f0 i2))
         (irem i2 'f0-curve)  
         (irem i1 'f0-curve) )
     (setq *i* i1))))  
  (each-note-if   ;rem lowest of two max
   (i?next (1- *i*) 'f0-curve)
   (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve)
   (eq 'max (iget (i?next (1- *i*) 'f0-curve) 'f0-curve))
   (eq 'max (iget (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve) 'f0-curve))
   (then
    (let 
     ((i1 (i?next (1- *i*) 'f0-curve)) 
      (i2 (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve)))
     (if (> (iget-f0 i1)(iget-f0 i2))
         (irem i2 'f0-curve)  
         (irem i1 'f0-curve) )
     (setq *i* i1))))  
     )

(defun pitch-acc-mark-ddr (quant)
 (let ((upval (* quant -0.07)) ;maximum deviation uphill
       (downval (* quant -0.05)) ;maximum deviation downhill
       (start-i 0)
       (end-i 0)
       (middle-i 0)
       (drsum 0.) )
  (each-note-if
   (i?next (1- *i*) 'f0-curve)
   (i?next (i?next (1- *i*) 'f0-curve) 'f0-curve)
   (then 
     (setq start-i (i?next (1- *i*) 'f0-curve))
     (setq end-i (i?next start-i 'f0-curve))
     (setq drsum (+ (drsum start-i end-i) (iget end-i 'dr)))
     (cond
        ((and
           (> (- end-i start-i) 3) 
           (eq (iget start-i 'f0-curve) 'min)        ;upslope
           (eq (iget end-i 'f0-curve) 'max) )
         (setq middle-i (i?drsum start-i (/ drsum 2.)))
         (if (= middle-i start-i)(incf middle-i))
        (if *rule-debug-info* (print-ll "min-max" "start " start-i " middle " middle-i " end " end-i))
         (iramp-mean-new-decimal (1+ start-i) middle-i 0.0 upval 'ddr)
         (iramp-mean-new-decimal middle-i end-i upval 0.0 'ddr) )
        ((and 
           (> (- end-i start-i) 3) 
           (eq (iget start-i 'f0-curve) 'max)        ;downslope
           (eq (iget end-i 'f0-curve) 'min) )
         (setq middle-i (i?drsum start-i (/ drsum 2.)))
         (if (= middle-i start-i)(incf middle-i))
        (print-ll "max-min" "start " start-i " middle " middle-i " end " end-i)
         (iramp-mean-new-decimal (1+ start-i) middle-i 0.0 downval 'ddr)
         (iramp-mean-new-decimal middle-i end-i downval 0.0 'ddr) ))
     (setq *i* start-i)            
     ))))

(defun pitch-acc-apply ()
 (each-note-if
   (this 'ddr)
   (set-this 'dr (* (this 'dr) (+ 1. (this 'ddr)))) ))

;(defun foo ()(pitch-acc-mark-f0-mean-5)(pitch-acc-mark-min-max))


;---------- pitch acc on phrases and subphrases-----------

(defun pitch-acc-phrase-lin (quant)
  (pitch-acc-phrase quant :shape :lin))

(defun pitch-acc-phrase-cos (quant)
  (pitch-acc-phrase quant :shape :cos))

(defun pitch-acc-phrase-sqrt (quant)
  (pitch-acc-phrase quant :shape :sqrt))

(defun pitch-acc-phrase-sqrt2 (quant)
  (pitch-acc-phrase quant :shape :sqrt2))

(defun pitch-acc-phrase-sqrt3 (quant)
  (pitch-acc-phrase quant :shape :sqrt3))

(defun pitch-acc-phrase-x2 (quant &key (power 2))
  (pitch-acc-phrase quant :shape :x2 :power power))

(defun pitch-acc-phrase-x2 (quant &key (power 2))
  (setq *phrase-start-name* 'phrase-start)
  (setq *phrase-end-name* 'phrase-end)
  (pitch-acc-phrase quant :shape :x2 :power power))

(defun pitch-acc-motive-x2 (quant &key (power 2))
  (setq *phrase-start-name* 'motive-start)
  (setq *phrase-end-name* 'motive-end)
  (pitch-acc-phrase quant :shape :x2 :power power))

(defun pitch-acc-phrase-x3 (quant &key (power 3))
  (setq *phrase-start-name* 'phrase-start)
  (setq *phrase-end-name* 'phrase-end)
  (pitch-acc-phrase quant :shape :x2 :power power))

(defun pitch-acc-motive-x3 (quant &key (power 3))
  (setq *phrase-start-name* 'motive-start)
  (setq *phrase-end-name* 'motive-end)
  (pitch-acc-phrase quant :shape :x2 :power power))

(defun pitch-acc-phrase-x4 (quant &key (power 4))
  (setq *phrase-start-name* 'phrase-start)
  (setq *phrase-end-name* 'phrase-end)
  (pitch-acc-phrase quant :shape :x2 :power power))

(defun pitch-acc-motive-x4 (quant &key (power 4))
  (setq *phrase-start-name* 'motive-start)
  (setq *phrase-end-name* 'motive-end)
  (pitch-acc-phrase quant :shape :x2 :power power))


(defun pitch-acc-phrase (quant &key shape power)
   ;(pitch-acc-skip-rests)
   (pitch-acc-phrase-mark-min-max)
   ;(pitch-acc-rem-min-min)
   (pitch-acc-phrase-mark-ddr quant :shape shape :power power)
   (pitch-acc-phrase-apply)
   ;(pitch-acc-mark-rests)
         )

(defvar *phrase-start-name*)
(defvar *phrase-end-name*)
(setq *phrase-start-name* 'phrase-start)
(setq *phrase-end-name* 'phrase-end)
(setq *phrase-start-name* 'motive-start)
(setq *phrase-end-name* 'motive-end)

(defun pitch-acc-phrase-mark-min-max ()
 (each-note-if
    (this *phrase-start-name*)
    (then
       ;(print *i*)
       (let ((istart *i*)
             (iend (i?next *i* *phrase-end-name*))
             (fmin 128)(fmax 0)(imax)(imin) )
         ;(print-ll istart "  " iend)
         (loop for i from istart to iend do
            (if (> (iget i 'f0) fmax)
                (progn (setq fmax (iget i 'f0))
                       (setq imax i)))
            (if (<= (iget i 'f0) fmin)
                (progn (setq fmin (iget i 'f0))
                       (setq imin i)) ))

          ;tone rep. the last two?
         (if (and (= (1+ imax) iend)
                  (= (iget imax 'f0) (iget iend 'f0)))
             (setq imax iend) )
         (if (and (= (1+ imin) iend)
                  (= (iget imin 'f0) (iget iend 'f0)))
             (setq imin iend) )

         ;if max second last and more than 3 notes, move to last
         (if (and (= (1+ imax) iend)
                  (> (- iend istart) 3))
             (setq imax iend) )
         (if (and (= (1+ imin) iend)
                  (> (- iend istart) 3))
             (setq imin iend) )
         
         ;if max second note and more than 3 notes, move to first
         (if (and (= (1- imax) istart)
                  (> (- iend istart) 3))
             (setq imax istart) )
         (if (and (= (1- imin) istart)
                  (> (- iend istart) 3))
             (setq imin istart) )

         (iset imax 'f0-curve 'max)
         (iset imin 'f0-curve 'min)
         (setq *i* iend)
           ))))

(defun pitch-acc-phrase-mark-ddr (quant)
 (let ((peakval-ddr (* quant -0.07))
       (peakval-l0 (* quant 3)) )
 (each-note-if
    (this *phrase-start-name*)
    (then
       (print *i*)
       (let ((istart *i*)
             (iend (i?next *i* *phrase-end-name*))
             (ipeak (i?next *i* 'f0-curve)) )
         (print-ll istart "  " iend "  " ipeak)
         (if (>= ipeak iend)
             (setq ipeak (+ istart (round (/ (float (- iend istart)) 2.)))) )
         (if (and (i?next ipeak 'f0-curve)
                  (< (i?next ipeak 'f0-curve) iend))
             (setq ipeak (i?next ipeak 'f0-curve)) )
         (iramp-new-decimal istart ipeak 0.0 peakval-ddr 'ddr)
         (iramp-new-decimal ipeak iend peakval-ddr 0.0 'ddr)
         (iramp-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
         (iramp-new-decimal ipeak iend peakval-l0 0.0 'dl0)
         (setq *i* iend)
           )))))

(defun pitch-acc-phrase-mark-ddr (quant &key shape)
  (let ((curve (or shape :sqrt2))) ; :cos :lin :sqrt :sqrt2
    (print-ll "pitch-acc-phrase-mark-ddr : shape = " curve)
    (each-note-if
      (this *phrase-start-name*)
      (then
        (let ((peakval-tempo (* quant 0.2))  ;quant scaling parameters
              (peakval-l0 (* quant 4))
              (down-slope-scaling 0.7) )
          ;(print *i*)
          (let ((istart *i*)
                (iend (i?next *i* *phrase-end-name*))
                (ipeak (i?next *i* 'f0-curve)) )
            (print-ll istart "  " iend "  " ipeak)
            
            ;scale quant rel duration
            (let ((drtot 0.))
              (loop for i from istart to iend do
                   (setq drtot (+ drtot (this 'dr))) )
              (setq peakval-tempo (* peakval-tempo (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0)))
              (setq peakval-l0 (* peakval-l0 (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0))) )
            
            ;scale quant rel downslope
            (if (< (iget iend 'f0) (iget istart 'f0))
              (progn
                (print "down")
                (setq peakval-tempo (* peakval-tempo down-slope-scaling))
                (setq peakval-l0 (* peakval-l0 down-slope-scaling)) ))
            
            ;put ipeak in the middle or first right of middle if no min/max
            (if (or (not ipeak)(>= ipeak iend))
              (setq ipeak (+ istart (round (+ -0.25 (/ (float (- (1+ iend) istart)) 2.))))) )

            ;take last peak if several
            (if (and (i?next ipeak 'f0-curve)
                     (< (i?next ipeak 'f0-curve) iend))
              (setq ipeak (i?next ipeak 'f0-curve)) )
            (cond 
             ((eq :cos curve)
              ;(print "cos interpolation")
              (iramp-cos-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-cos-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-cos-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
              (iramp-cos-new-decimal ipeak iend peakval-l0 0.0 'dl0))
             ((eq :sqrt curve)
              ;(print "sqrt interpolation")
              (iramp-sqrt-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-sqrt-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-sqrt-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
              (iramp-sqrt-new-decimal ipeak iend peakval-l0 0.0 'dl0))
             ((eq :sqrt2 curve)
              ;(print "sqrt2 interpolation")
              (iramp-sqrt2-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-sqrt2-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-sqrt2-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
              (iramp-sqrt2-new-decimal ipeak iend peakval-l0 0.0 'dl0))
             ((eq :sqrt3 curve)
              ;(print "sqrt3 interpolation")
              (iramp-sqrt3-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-sqrt3-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-sqrt3-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
              (iramp-sqrt3-new-decimal ipeak iend peakval-l0 0.0 'dl0))
             ((eq :lin curve)
              ;(print "lin interpolation")
              (iramp-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
              (iramp-new-decimal ipeak iend peakval-l0 0.0 'dl0))
             ((eq :x2 curve)
              ;(print "x2 interpolation")
              (iramp-x2-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-x2-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-x2-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
              (iramp-x2-new-decimal ipeak iend peakval-l0 0.0 'dl0)))
            (setq *i* iend)
             ))))))

;bakvänt (gamla)
(defun pitch-acc-phrase-apply ()
 (each-note-if
   (this 'ddr)
   (set-this 'dr (* (this 'dr) (- 1. (this 'ddr)))))
 (each-note-if
   (this 'dl0)
   (add-this-l0 (this 'dl0)))
 )

(defun pitch-acc-phrase-mark-ddr (quant &key shape power)
  (let ((curve (or shape :sqrt2))) ; :cos :lin :sqrt :sqrt2
    (print-ll "pitch-acc-phrase-mark-ddr : shape = " curve)
    (each-note-if
      (this *phrase-start-name*)
      (then
        (let ((peakval-tempo (- (* quant 0.2)))  ;quant scaling parameters
              (peakval-l0 (* quant 4))
              (down-slope-scaling 0.7) )
          ;(print *i*)
          (let ((istart *i*)
                (iend (i?next *i* *phrase-end-name*))
                (ipeak (i?next *i* 'f0-curve)) )
            (print-ll istart "  " iend "  " ipeak)
            
            ;scale quant rel duration
            (let ((drtot 0.))
              (loop for i from istart to iend do
                   (setq drtot (+ drtot (this 'dr))) )
              (setq peakval-tempo (* peakval-tempo (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0)))
              (setq peakval-l0 (* peakval-l0 (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0))) )
            
            ;scale quant rel downslope
            (if (< (iget iend 'f0) (iget istart 'f0))
              (progn
                (print "down")
                (setq peakval-tempo (* peakval-tempo down-slope-scaling))
                (setq peakval-l0 (* peakval-l0 down-slope-scaling)) ))
            
            ;put ipeak in the middle or first right of middle if no min/max
            (if (or (not ipeak)(>= ipeak iend))
              (setq ipeak (+ istart (round (+ -0.25 (/ (float (- (1+ iend) istart)) 2.))))) )

            ;take last peak if several
            (if (and (i?next ipeak 'f0-curve)
                     (< (i?next ipeak 'f0-curve) iend))
              (setq ipeak (i?next ipeak 'f0-curve)) )

            ;if peak is close to iend move one note to the middle
            (if (and (>= (- iend istart) 6)
                     (<= (- iend ipeak) 2) )
              (decf ipeak))
                     
            (cond 
             ((eq :cos curve)
              ;(print "cos interpolation")
              (iramp-cos-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-cos-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-cos-new-decimal istart ipeak 0.0 (- peakval-l0) 'dl0)
              (iramp-cos-new-decimal ipeak iend (- peakval-l0) 0.0 'dl0))
             ((eq :lin curve)
              ;(print "lin interpolation")
              (iramp-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
              (iramp-new-decimal ipeak iend peakval-tempo 0.0 'ddr)
              (iramp-new-decimal istart ipeak 0.0 (- peakval-l0) 'dl0)
              (iramp-new-decimal ipeak iend (- peakval-l0) 0.0 'dl0))
             ((eq :x2 curve)
              ;(print "x2 interpolation")
              (iset-ramp-x2-decimal-last istart ipeak 0.0 peakval-tempo 'ddr :power power)
              (iset-ramp-x2-decimal-last ipeak iend peakval-tempo 0.0 'ddr :power power)
              (iset-ramp-x2-decimal-last istart ipeak 0.0 (- peakval-l0) 'dl0 :power power)
              (iset-ramp-x2-decimal-last ipeak iend (- peakval-l0) 0.0 'dl0 :power power)))
            (setq *i* iend)
             ))))))
(defun pitch-acc-phrase-mark-ddr (quant &key shape power)
  (let ((curve (or shape :sqrt2))) ; :cos :lin :sqrt :sqrt2
    (print-ll "pitch-acc-phrase-mark-ddr : shape = " curve)
    (each-note-if
      (this *phrase-start-name*)
      (then
        (let ((peakval-tempo (- (* quant 0.2)))  ;quant scaling parameters
              (peakval-l0 (* quant 4))
              (down-slope-scaling 0.7) )
          ;(print *i*)
          (let ((istart *i*)
                (iend (i?next *i* *phrase-end-name*))
                (ipeak (i?next *i* 'f0-curve)) )
            (print-ll istart "  " iend "  " ipeak)
            
            ;scale quant rel duration
            (let ((drtot 0.))
              (loop for i from istart to iend do
                   (setq drtot (+ drtot (this 'dr))) )
              (setq peakval-tempo (* peakval-tempo (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0)))
              (setq peakval-l0 (* peakval-l0 (dr-linear-limits drtot 1500.0 0.5 5000.0 1.0))) )
            
            ;scale quant rel downslope
            (if (< (iget iend 'f0) (iget istart 'f0))
              (progn
                (print "down")
                (setq peakval-tempo (* peakval-tempo down-slope-scaling))
                (setq peakval-l0 (* peakval-l0 down-slope-scaling)) ))
            
            ;put ipeak in the middle or first right of middle if no min/max
            (if (or (not ipeak)(>= ipeak iend))
              (setq ipeak (+ istart (round (+ -0.25 (/ (float (- (1+ iend) istart)) 2.))))) )

            ;take last peak if several
            (if (and (i?next ipeak 'f0-curve)
                     (< (i?next ipeak 'f0-curve) iend))
              (setq ipeak (i?next ipeak 'f0-curve)) )

            ;if peak is close to iend move one note to the middle
            (if (and (>= (- iend istart) 6)
                     (<= (- iend ipeak) 2) )
              (decf ipeak))

            ;always in the middle
            (setq ipeak (+ istart (round (+ -0.25 (/ (float (- (1+ iend) istart)) 2.)))))

            (setq peakval-l0 (- peakval-l0))
            (let ((peakval-tempo-end (* (- peakval-tempo) 0.5)) ;scaling of last segment
                  (peakval-l0-end (* (- peakval-l0) 0.5)) )
              (cond 
               ((eq :cos curve)
                ;(print "cos interpolation")
                (iramp-cos-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
                (iramp-cos-new-decimal ipeak iend peakval-tempo peakval-tempo-end 'ddr)
                (iramp-cos-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
                (iramp-cos-new-decimal ipeak iend peakval-l0 peakval-l0-end 'dl0))
               ((eq :lin curve)
                ;(print "lin interpolation")
                (iramp-new-decimal istart ipeak 0.0 peakval-tempo 'ddr)
                (iramp-new-decimal ipeak iend peakval-tempo peakval-tempo-end 'ddr)
                (iramp-new-decimal istart ipeak 0.0 peakval-l0 'dl0)
                (iramp-new-decimal ipeak iend peakval-l0 peakval-l0-end 'dl0))
               ((eq :x2 curve)
                ;(print "x2 interpolation")
                (iset-ramp-x2-decimal-last istart ipeak 0.0 peakval-tempo 'ddr :power power)
                (iset-ramp-x2-decimal-last ipeak iend peakval-tempo peakval-tempo-end 'ddr :power power)
                (iset-ramp-x2-decimal-last istart ipeak 0.0 peakval-l0 'dl0 :power power)
                (iset-ramp-x2-decimal-last ipeak iend peakval-l0 peakval-l0-end 'dl0 :power power)))
              (setq *i* iend)
              )))))))

  (defun pitch-acc-phrase-apply ()
 (each-note-if
   (this 'ddr)
   (set-this 'dr (* (this 'dr) (+ 1. (this 'ddr)))))
 (each-note-if
   (this 'dl0)
   (add-this-l0 (- (this 'dl0))))
 )
|#


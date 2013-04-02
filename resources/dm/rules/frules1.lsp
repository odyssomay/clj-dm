;;;-*-Mode: LISP; Package: DM -*-

;;the Fryden-Sundberg rules

;;1986-89/Anders Friberg 
;;9201 cl /af
;;920519 Durational contrast short between long

(in-package "DM")

;------ Melodic charge---------------------------

;;0009/af change of error message
(defun melodic-charge (quant &key (amp 1)(dur 1)(vibamp 1))
  (if (not (get-first 'q)) 
    (Print-ll "Melodic-charge : no chord on first note - skipping rule")
    (progn 
      (melodic-charge-dr (* dur quant))
      (melodic-charge-amp)
      (melodic-charge-smear)
      (melodic-charge-vib (* vibamp quant))
      (each-note-if
        (this 'msl)
        (then
          (add-this 'sl (* amp quant (this 'msl)))
          (rem-this 'msl) )))))

;dL = 0.2*CM*quant (dB)
;CM = melodic-charge
;;0009/af
(defun melodic-charge-amp ()
 (let ((qnr))
  (each-note 
     (if (this 'q)
         (setq qnr (tone-to-tonnr (chord-to-root (this 'q)))) )
   (ifn (this 'rest)
    (set-this 'msl
              (*
                 0.2
                 (melodic-charge-fn
                   (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12)
                  )))))))

;;0009/af take the first note in the chord list or just the name if it is a string 
(defun chord-to-root (q)
  (if (stringp q) (setq q (qname-to-qlist q)))
  (car q) )



;symmetrical version  **** no listening test done ******       
#|
(defun melodic-charge-smear ()
 (each-note-if
     (this 'mda0)
     (prev 'mda0)
     (next 'mda0)
     (then
        (cond 
           ((> (this 'mda0) (* (prev 'mda0) 2))   ; uphill
            (set-prev 'mda0 (round (* 0.75 (this 'mda0))) )))
        (cond 
           ((> (this 'mda0) (* (next 'mda0) 2))   ; downhill
            (set-next 'mda0 (round (* 0.55 (this 'mda0))) )))
     )))
|#        
;smears out the peaks
;ej symmetrisk ej ok på hkyrie takt 7     
(defun melodic-charge-smear ()
 (each-note-if
   (not (first?))
   (not (last?))
     (this 'msl)
     (prev 'msl)
     (next 'msl)
     (< (prev 'msl) (* (this 'msl) 2))
     (< (abs (- (prev-f0) (this-f0))) 3)  ;less than a third to next note
     (not (= (prev-f0) (this-f0)))        ;not equal
     (= (prev 'ndr) (this 'ndr))            ;same duration
     (< (prev 'ndr) 500)
     (then
        (set-prev 'msl (round (* 0.75 (this 'msl))))
        (if (> (this 'msl) (* (next 'msl) 2))
            (set-next 'msl (round (* 0.55 (this 'msl))))
        ))))

;dr = dr*(1 + (2/300)*CM*quant)
;;0009 included chord-to-root/af
(defun melodic-charge-dr (quant)
 (let ((qnr))
  (each-note 
     (if (this 'q)
         (setq qnr (tone-to-tonnr (chord-to-root (this 'q)))) )
   (ifn (this 'rest)
    (set-this 'dr
       (* 
         (this 'dr)
         (+ 1
            (* quant
               (/ 2. 300.)
               (melodic-charge-fn
                 (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12) ))))
              )))))

#|
; in  percent
;gives va = (0.3/2)*(mda0/10)*10*quant
;assumes va = 127 gives 12.5 percent vibrato
;set the value only if changed and on first note
;*******the quant is already in the rule above --fixat*********
(defun melodic-charge-vib ()
 (let ((last-va 0)(base-vib 3))
  (each-note-if
     (this 'mda0)
     (then
       (let ((va (round (* 0.15 10 (da0-to-l0 (this 'mda0))))))
         (if (first?) (set-this 'va base-vib) )
         (if (/= last-va va)
           (then 
             (set-this 'va (+ va base-vib))
             (setq last-va (+ va base-vib))
                    )))))))
|#
;set the vibrato amplitude from mda0
;vibrato amplitude = 0.03*CM (%)
;gives va = (0.3/2)*(mda0/10)*16.8*quant in cent
;set the value only if changed and on first note
;*******the quant is already in the rule above --fixat*********
(defun melodic-charge-vib (quant)
 (let ((last-va 0)(base-vib 3))
  (each-note-if
     (this 'msl)
     (then
       (let ((va (round (* quant 0.15 16.8 (this 'msl)))))
         (if (first?) (set-this 'va base-vib) )
         (if (/= last-va va)
           (then 
             (set-this 'va (+ va base-vib))
             (setq last-va (+ va base-vib))
                    )))))))

;no care of prog change
(defun rem-not-changed (prop)
 (let ((last-prop -30000))
  (each-note-if
     (this prop)
     (then
        ;(print (this prop) last-prop)
        (if (= (this prop) last-prop)
            (then
               (setq last-prop (this prop))
               (rem-this prop) )
            (setq last-prop (this prop))
            )))))

;CM   
(defun melodic-charge-fn (nr)
    (case nr
      (0 0) (1 6.5) (2 2) (3 4.5) (4 4)
      (5 2.5) (6 6) (7 1) (8 5.5) (9 3) (10 3.5) (11 5)
       ))  
(defun signed-melodic-charge-fn (nr)
    (case nr
      (0 0) (1 -6.5) (2 2) (3 -4.5) (4 4)
      (5 -2.5) (6 6) (7 1) (8 -5.5) (9 3) (10 -3.5) (11 5)
       ))  

;mark melodic charge values in the prop :mc
;;0009/af added warning
(defun mark-melodic-charge ()
  (let ((qnr))
    (if (not (and (get-first 'key) (get-first 'q)))
        (print "MARK-MELODIC-CHARGE: no melodic charge marked - missing q or key in the first note")
      (each-note 
       (if (this 'q)
           (setq qnr (tone-to-tonnr (chord-to-root (this 'q)))) )
       (ifn (this 'rest)
            (set-this :mc
                      (melodic-charge-fn
                       (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12)
                       ))))))) 


;;0009/af
(defun mark-signed-melodic-charge ()
 (block markmc
 (let ((qnr))
  (each-note 
     (if (and (first?) (not (this 'q)))(return-from markmc))
     (if (this 'q)
         (setq qnr (tone-to-tonnr (chord-to-root (this 'q)))) )
   (ifn (this 'rest)
    (set-this :mc
        (signed-melodic-charge-fn
             (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12)
             )))))))
  

;----- Harmonic charge---------------

;;0009/af change of error message
(defun harmonic-charge (quant &key (amp 1)(dur 1)(vibfreq 1))
  (if (or (not (get-first 'q)) (not (get-first 'key))) 
    (Print-ll "Harmonic-charge : no chord or key on first note - skipping rule")
    (progn 
      (harmonic-charge-amp quant amp)
      (harmonic-charge-dr dur)
      (if (not (zerop vibfreq)) (harmonic-charge-vf vibfreq))
      (harmonic-charge-tempo (* dur quant))
      (harmonic-charge-first (* dur quant))
      (each-note (rem-this 'hsl)   ;clean up
                 (rem-this 'chdl))
      )))

(defun harmonic-charge-amp (quant ampscale)
  (harmonic-charge-amp1 quant)
  (harmonic-charge-amp2)
  (harmonic-charge-amp3 ampscale)
  )

;*****fixa om ej ett ackord står sist!!!!!*********
;set the target-values on the notes with q-value
;chdl is the value to be added to l0
;chdl = 1.5*sqrt(CH)*quant (dB)
;ch = harmonic charge
(defun harmonic-charge-amp1 (quant)
  (let ((key))
    (each-note 
     (if (this 'key) (setq key (this 'key)))
     (cond ((this 'q)
            (set-this 'chdl
                      (* quant 1.5 (sqrt (harmonic-charge-fn (this 'q) key nil)))
                      ))))))

;0911/af fixed if a chord is not last
(defun harmonic-charge-amp1 (quant)
  (let ((key)(chdl-val))
    (each-note 
     (if (this 'key) (setq key (this 'key)))
     (when (this 'q)
       (setq chdl-val (* quant 1.5 (sqrt (harmonic-charge-fn (this 'q) key nil))))
       (set-this 'chdl chdl-val) )
     (if (last?) (set-this 'chdl chdl-val)) ;set again the last value on the last note
     )))

;makes the ramp between the target values
;the crescendo starts 1.9 s before the target
#|
;with cosine interpolation 
(defun harmonic-charge-amp2 ()
(let ((this-chdl) (nexti-chdl))
 (each-note  
   (cond
     ((and (setq this-chdl (this 'chdl))
           (setq nexti-chdl (i?next *i* 'chdl)) )
      (let* ((next-chdl (iget nexti-chdl 'chdl))
             (chdldiff (- next-chdl this-chdl)) )
         (cond
           ((minusp chdldiff)          ;decrescendo
            (iramp-cos-mean-new *i* nexti-chdl this-chdl next-chdl 'hda0) )
           ((plusp chdldiff)           ;crescendo 
              (until (< (drsum *i* nexti-chdl) 1900.) 
                     (iset *i* 'hsl this-chdl) 
                     (incf *i*) )
            (iramp-cos-mean-new *i* nexti-chdl this-chdl next-chdl 'hsl) ))))
       ((last?)
        (set-this 'hsl (this 'chdl)) )))))
;with linear interpolation
(defun harmonic-charge-amp2 ()
(let ((this-chdl) (nexti-chdl))
 (each-note  
   (cond
     ((and (setq this-chdl (this 'chdl))
           (setq nexti-chdl (i?next *i* 'chdl)) )
      (let* ((next-chdl (iget nexti-chdl 'chdl))
             (chdldiff (- next-chdl this-chdl)) )
         (cond
           ((minusp chdldiff)          ;decrescendo
            (iramp-mean-new *i* nexti-chdl this-chdl next-chdl 'hsl) )
           ((plusp chdldiff)           ;crescendo 
              (until (< (drsum *i* nexti-chdl) 1900.) 
                     (iset *i* 'hsl this-chdl) 
                     (incf *i*) )
            (iramp-mean-new *i* nexti-chdl this-chdl next-chdl 'hsl) ))))
       ((last?)
        (set-this 'hsl (this 'chdl)) )))))
|#

;with linear interpolation
(defun harmonic-charge-amp2 ()
(let ((this-chdl) (nexti-chdl))
 (each-note-if  
   (cond
     ((and (setq this-chdl (this 'chdl))
           (setq nexti-chdl (i?next *i* 'chdl)) )
      (let* ((next-chdl (iget nexti-chdl 'chdl))
             (chdldiff (- next-chdl this-chdl)) )
         (cond
           ((or (minusp chdldiff)          ;decrescendo
                (zerop chdldiff))
            (iramp-new-decimal *i* nexti-chdl this-chdl next-chdl 'hsl) )
           ((plusp chdldiff)           ;crescendo 
              (until (< (drsum *i* nexti-chdl) 1900.) 
                     (iset *i* 'hsl this-chdl) 
                     (incf *i*) )
            (iramp-new-decimal *i* nexti-chdl this-chdl next-chdl 'hsl) ))))
       ((last?)
        (set-this 'hsl (this 'chdl)) )))))

;add  da0 to a0
(defun harmonic-charge-amp3 (ampscale)
  (each-note-if
    (this 'rest)
    (rem-this 'hsl) )
  (each-note-if
     (this 'hsl)
     (add-this 'sl (* ampscale (this 'hsl))) ))

;duration in proportion to sl above
;normal: quant = 1
;dr = dr*sqrt(1+0.018*quant*hsl)
;*******the quant is already in the rule above --fixat*********
(defun harmonic-charge-dr (durscale)
  (each-note 
    (cond ((this 'hsl)
           (let ((newdr
                  (* (this 'dr)
                     (sqrt (+ (* (this 'hsl) ;to dB
                                 0.018)
                              1) ))))
             (add-this 'dr (* durscale (- newdr (this 'dr))))
             )))))

;vf in proportion to dda0 above
;normal: quant = 1
;vf = 10*(5 + 0.81*hda0/10)
#| old version
(defun harmonic-charge-vf ()
  (each-note-if 
      (this 'hsl)
      (set-this 'vf
         (round
            (* 10 (+ 5 (* 0.81
                          (this 'hsl)) ))))))
|#
; with vf in Hertz
(defun harmonic-charge-vf (vibfreqscale)
  (each-note-if 
      (this 'hsl)
      (set-this 'vf
            (+ 5 (* 0.81 vibfreqscale (this 'hsl))) )))
 
; dr = dr + 2*sqrt(ch)*quant for each note
(defun harmonic-charge-tempo (quant)
 (let ((xdr)(key))
  (each-note 
     (if (this 'key) (setq key (this 'key)))
     (if (this 'q)
         (setq xdr (* quant 2.0  
                      (sqrt (harmonic-charge-fn (this 'q) key nil)) )))
     (set-this 'dr
               (+ (this 'dr) xdr) ))))

; dr = dr + 8*sqrt(ch)*quant for just the first note in a chord change 
(defun harmonic-charge-first (quant)
 (let ((key))
  (each-note 
     (if (this 'key) (setq key (this 'key)))
     (if (this 'q)
         (then
            (set-this 'dr 
               (+ (this 'dr) 
                  (* quant 8.0  
                     (sqrt (harmonic-charge-fn (this 'q) key nil)) )))))
  )))


;ackback aux. functions

;returns the tension of the chord in qlist relative key (float)
;modus is not used
;CH = 2*(CM,I/2 + CM,III/3 + CM,V/6) - 3
;CH = harmonic charge
;CM = melodic charge
(defun harmonic-charge-compute (qlist key modus)
  (declare (ignore modus))
    (if (stringp qlist) (setq qlist (qname-to-qlist qlist)))
    (setq qlist (mapcar #'tone-to-tonnr qlist))
    (setq qlist
       (mapcar '-
                qlist
                (make-list (length qlist) :initial-element (tone-to-tonnr key))))
    (setq qlist
       (mapcar '(lambda (nr)
                  (while (< nr 0) (incf nr 12))
                  (while (> nr 12) (decf nr 12))
                  nr )
                qlist) )
    (setq qlist (mapcar #'melodic-charge-fn qlist))
    (let
      ((ch (+ (car qlist) ;compute the value from the first 3 notes in the chord
              (* (/ 2. 3.) (cadr qlist))
              (* (/ 1. 3.) (caddr qlist))
              -3 )))
      (if (minusp ch) 0. ch)  ;lower limit is 0
     ))

(defun harmonic-charge-fn (qlist key modus)
  (ifn key (error "harmonic-charge-fn: no key"))
  (if (stringp qlist) (setq qlist (qname-to-qlist qlist)))
  (let* ((root (tone-to-tonnr (car qlist)))
         (keydist (- root (tone-to-tonnr key)))
         (chord-type (chord-type (qlist-to-tonnrlist qlist))) )
    (while (< keydist 0) (incf keydist 12))
    (while (> keydist 12) (decf keydist 12))
    (if (equal chord-type "dim")
      5                        ;same value for all dim
      (let ((l (list keydist chord-type)))
        (cond
         ((equal l '(0 "maj")) 0)
         ((equal l '(0 "min")) 0.3)
         ((equal l '(1 "maj")) 7)
         ((equal l '(1 "min")) 8) 
         ((equal l '(2 "maj")) 4)
         ((equal l '(2 "min")) 2.5)
         ((equal l '(3 "maj")) 3.5)
         ((equal l '(3 "min")) 7)
         ((equal l '(4 "maj")) 6)
         ((equal l '(4 "min")) 3)
         ((equal l '(5 "maj")) 3)
         ((equal l '(5 "min")) 3.5)
         ((equal l '(6 "maj")) 7.5)
         ((equal l '(6 "min")) 7)
         ((equal l '(7 "maj")) 1.5)
         ((equal l '(7 "min")) 2) 
         ((equal l '(8 "maj")) 4)
         ((equal l '(8 "min")) 6.5)
         ((equal l '(9 "maj")) 6)
         ((equal l '(9 "min")) 1.5)
         ((equal l '(10 "maj")) 3.5)
         ((equal l '(10 "min")) 6)
         ((equal l '(11 "maj")) 7)
         ((equal l '(11 "min")) 5.5)
         (t                         ;if not in list compute in old style
          (print-ll "warning: chord not in list: " qlist) 
          (harmonic-charge-compute qlist key modus) )
         )))))


(defun chord-type (l)
 (cond
   ((equal l '(0 4 7)) "maj")
   ((equal l '(0 3 7)) "min")
   ((equal l '(0 3 6)) "dim") ))



;;(defun foo () (harmonic-charge-fn '("G" "C" "E") "C" "maj") )

(defun harmonic-charge-table ()
  (let ((chordl '("C" "Cm" "C#" "C#m" "D" "Dm" "D#" "D#m" "E" "Em"
                  "F" "Fm" "F#" "F#m" "G" "Gm" "G#" "G#m" "A" "Am"
                  "A#" "A#m" "B" "Bm" )))
    (mapc #'(lambda (qname)
             (print-ll qname "    " 
               (harmonic-charge-fn (qname-to-qlist qname) "C" "maj") ))
            chordl )))


#|
;only mark the harmonic charge value in each note
(defun mark-harmonic-charge ()
  (mark-harmonic-charge-amp1)
  (harmonic-charge-amp2)
  (each-note
    (set-this :hc (this 'hda0)))
  ;(rem-all 'hda0)
  ;(rem-all 'chdl)
  )

  (defun mark-harmonic-charge-amp1 ()
    (let ((key))
      (each-note 
        (if (this 'key) (setq key (this 'key)))
        (cond ((this 'q)
               (set-this 'chdl
                         (harmonic-charge-fn (this 'q) key nil))))
        )))
|#
;mark Harmonic charge value on each new chord
(defun mark-harmonic-charge ()
  (let ((key))
    (each-note 
      (if (this 'key) (setq key (this 'key)))
      (cond ((this 'q)
             (set-this :hc
                       (harmonic-charge-fn (this 'q) key nil))))
      )))


;;---------------- Marking chromatic charge --------------

;;normal Quant = 1
(defun chromatic-charge (quant)
  (chrom-charge-mean-interval)
  (chrom-charge-modulo-octave)
  (chrom-charge-five-mean)
  (chrom-charge-set-values quant)
  (rem-all 'cchrom)
  (rem-all 'cchrom-mean)
   )

;(defun foo ()(chrom-charge-mean-interval)(chrom-charge-modulo-octave) )

#|
(defun chromatic-charge-random (quant)
  (chrom-charge-mean-interval-random)
  (chrom-charge-modulo-octave)
  (chrom-charge-five-mean)
  (chrom-charge-set-values quant)
  ;(rem-all 'cchrom)
  ;(rem-all 'cchrom-mean)
   )

(defun foo () (chromatic-charge-random 1))
|#

;cchrom = 32 - |semit to next note jumping over rests|
(defun chrom-charge-mean-interval ()
  (block last
   (let ((max 32))
    (each-note-if
      (not (last?))
      (not (this 'rest))
      (then
        (let ((i (i?next-note *i*)))
          ;(if i (print (this 'n) (iget i 'n)))
          (if i (set-this 'cchrom 
                          (- max (abs (- (this-f0)
                                         (if (listp (iget i 'f0))
                                           (car (reverse (iget i 'f0)))
                                           (iget i 'f0) ))))))
          ))))
  (each-note-if ;set the last note as previous
    (last?)
    (then
        (let* ((i2 (i?prev-note (1+ *i*)))
               (i1 (i?prev-note i2)) )
           (iset i2 'cchrom (iget i1 'cchrom)) )
        (return-from last) ))
 ))

#|         
(defun chrom-charge-mean-interval-random ()
   (let ((max 32))
    (each-note-if
      (not (this 'rest))
      (then
          (set-this 'cchrom (random 0 max))
          ))))
|#
         
;cchrom = cchrom mod 12
(defun chrom-charge-modulo-octave ()
   (each-note-if
     (this 'cchrom)
     (then
        (set-this 'cchrom (- (mod (this 'cchrom) 12) 6))
        )))

#|
(defun chrom-charge-tre-mean ()
  (each-note-if
     (this 'min)
     (i?prev *i* 'min)
     (i?next *i* 'min)
     (then
        (set-this 'mi 
            (/ (+ 
                (iget (i?prev *i* 'min) 'min)
                (this 'min) 
                (iget (i?next *i* 'min) 'min) )
               3. )))))
|#

;'cchrom-mean is the mean of 5 'cchrom values
;two behind and two ahead 
(defun chrom-charge-five-mean ()
  (each-note-if
    (not (first?))
    (not (last?))
    (not (first+1?))
    (not (last-1?))
    (this 'cchrom)
    (i?prev *i* 'cchrom)
    (i?next *i* 'cchrom)
    (i?prev (i?prev *i* 'cchrom) 'cchrom)
    (i?next (i?next *i* 'cchrom) 'cchrom)
    (then
      (set-this 'cchrom-mean 
                (/ (+ 
                    (iget (i?prev (i?prev *i* 'cchrom) 'cchrom) 'cchrom)
                    (iget (i?prev *i* 'cchrom) 'cchrom)
                    (this 'cchrom) 
                    (iget (i?next *i* 'cchrom) 'cchrom)
                    (iget (i?next (i?next *i* 'cchrom) 'cchrom) 'cchrom) )
                   5. ))))
  (each-note-if ;first two notes
    (first?)
    (then
      (let ((i (i?next-note (1- *i*))))
        (iset i 'cchrom-mean (iget i 'cchrom))
        (setq i (i?next-note i))
        (iset i 'cchrom-mean (iget i 'cchrom))
        )))
  (each-note-if ;last two notes
    (last?)
    (then
      (let ((i (i?prev-note (1+ *i*))))
        (iset i 'cchrom-mean (iget i 'cchrom))
        (setq i (i?prev-note i))
        (iset i 'cchrom-mean (iget i 'cchrom))
        )))
  )

;set the performance parameters
;∆L = 1.35 * cchrom-mean * quant (dB)
;DR = DR*(1 + 0.009*cchrom-mean*quant)
(defun chrom-charge-set-values (quant)
  (each-note-if
     (this 'cchrom-mean)
     (then
        (add-this 'sl (* quant 1.35 (this 'cchrom-mean)) )
        (set-this 'dr
         (* 
           (this 'dr)
           (+ 1 (* quant 0.009 (this 'cchrom-mean)))
            )))))
         


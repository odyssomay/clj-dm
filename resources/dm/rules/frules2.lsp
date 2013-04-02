;;;-*-Mode: LISP; Package: DM -*-

;;the Fryden-Sundberg rules part II

;;1986-89/Anders Friberg 
;;9201/af translation to cl
;;920519 Durational contrast short between long
;;001006 DURATION-CONTRAST-ART, a new paramenter for repeated notes has been inserted
;;030427/af removed repetition-articulation-dro and rem-in-double-dur in duration-contrast-art
;;030427/af changed repetition-articulation-dro and removed "not if dro"
;;040427/af high-loud applied relative mean f0 in each track
;;091203/af new version high-loud2 with compressed amount



(in-package "DM")

;;-------- The higher the louder ----------------------
 
; 3 dB/octave assuming quant=1 and a0 = 1 dB / step
#|
(defun high-loud (quant)
  (each-note-if
    (not (this 'rest))
    (then
       (add-this 'sl
            (* quant
               (/ (- (this-f0) 60) 4.0)) ))))   ;taking C4 as zero
|#

;;applies deviation relative mean pitch for each track
;;thus no normalization neccessary
(defun high-loud (quant)
  (each-track
   (let ((i 0) 
         (f0-sum 0.0) (f0-mean 60))
     (each-note-if
      (this 'f0)
      (then
       (incf i)
       (incf f0-sum (this-f0))
       ))
     (setq f0-mean (/ f0-sum i))
     (each-note-if
      (this 'sl)
      (this 'f0)
      (then
       (add-this 'sl
                 (* quant
                    (/ (- (this-f0) f0-mean) 4.0)) )))
     )))

;;091203/af new version with compressed amount
;;square-root with factor 0.6 gives approximately 3dB difference in the
;;middle octave centered around mean
;; test with scale-cmaj-up-2oct.mus in the score/test folder
(defun high-loud2 (quant)
  (each-track
   (let ((i 0) (f0-sum 0.0) (f0-mean 60)(sl-add)(sl-sign))
     (each-note-if
      (this 'f0)
      (then
       (incf i)
       (incf f0-sum (this-f0)) ))
     (setq f0-mean (/ f0-sum i))
     (each-note-if
      (this 'sl)
      (this 'f0)
      (then
       (setq sl-add (* quant 0.60 (sqrt (abs (- (this-f0) f0-mean)))))
       (setq sl-sign (if (plusp (- (this-f0) f0-mean)) 1 -1))
       (add-this 'sl (* sl-sign sl-add))
       )))))
   
;;test for doing the same compression for all sound level variations
;;quant controls the exponent of the power function
;;quant=0 --> no change
;;quant=1 --> sqare root compression
;;0912/af
(defun compress-sl-pow (quant)
   (let ((sl-add)(sl-sign))
     (each-note-if
      (this 'sl)
      (this 'f0)
      (then
       (setq sl-add (expt (abs (this 'sl)) (/ 1.0 (+ 1 quant))))
       (setq sl-sign (if (plusp (this 'sl)) 1 -1))
       (set-this 'sl (* sl-sign sl-add))
       ))))

;;-------- The shorter the softer --------------------


#|
;l0=l0 - (120/dr)*quant (dB) with quant=1
(defun short-soft-old (quant)
  (each-note-if
    (not (this 'rest))
    (then 
        (add-this-l0 
            (- (* quant (/ 120. (this 'dr) ))) ))))


(defun short-soft (quant)
  (each-note-if
    (not (this 'rest))
    (< (this 'dr) 570)
    (> (this 'dr) 200)
    (then 
        (add-this-l0 
            (- (* quant (* (/ 0.5 370.) (- 570 (this 'dr)))))
            )))
  (each-note-if
    (not (this 'rest))
    (< (this 'dr) 201)
    (> (this 'dr) 30)
    (then 
        (add-this-l0 
            (- (* quant (* (/ 0.5 170.) (- (this 'dr) 30.))))
            )))
    )
|#

;;-------- Increase durational contrast ----------------

;;example: HaydnFdur

(defun duration-contrast (quant &key (amp 1)(dur 1))
 (duration-contrast1)
 ;(duration-contrast-rem-short-between-long)
 (duration-contrast-rem-double-duration)
 (duration-contrast-short-soft (* amp quant))
 (duration-contrast-short-short (* dur quant))
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr))
  )

;;piano specific
(defun duration-contrast-art (quant &key (rep 1))
  (duration-contrast1)
  ;(duration-contrast-rem-double-duration)
  (duration-contrast-short-short-art quant)
  
  ;(repetition-articulation-dro rep)
  
  (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr))
  )

;;;----
#|
;;use instead kewords above
(defun duration-contrast-dr (quant)
 (duration-contrast1)
 ;(duration-contrast-rem-short-between-long)
 (duration-contrast-rem-double-duration)
 ;(duration-contrast-short-soft quant)
 (duration-contrast-short-short quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr))
  )
;;change the duration proportionally from the average duration in
;;each voice
(defun duration-contrast-prop-dr (quant)
 (duration-contrast1-prop)
 (duration-contrast-rem-double-duration)
 (duration-contrast-short-short quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr))
  )
(defun duration-contrast-amp (quant)
 (duration-contrast1)
 ;(duration-contrast-rem-short-between-long)
 (duration-contrast-rem-double-duration)
 (duration-contrast-short-soft quant)
 ;(duration-contrast-short-short quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr))
  )
(defun duration-contrast-dr-all (quant)
 (duration-contrast1)
 (duration-contrast-short-short quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr))
  )
(defun duration-contrast-amp-all (quant)
 (duration-contrast1)
 (duration-contrast-short-soft quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr))
  )
|#

;max ddr = -10 ms
;; for 30 < DR < 201 => durcontddr = -(11/170)(dr - 30)
;; for 200 < DR < 401 => durcontddr = -(4/200)(400 - DR) - 7
;; for 400 < DR < 600 => durcontddr = -(7/200)(600 - DR)
(defun duration-contrast1 ()
 (each-note-if
  (not (this 'rest)) 
  (then
   (let ((dr (this 'dr)))
    (cond ((and
            (> dr 30)
            (< dr 201) )
           (then 
            (set-this 'durcontddr
              (infix (-11.0 / 170.0 * (dr - 30.0))) ))) 
          ((and
            (> dr 200)
            (< dr 401) )
           (then 
            (set-this 'durcontddr
              (infix (-4.0 / 200.0 * (400.0 - dr) - 7.0)) ))) 
          ((and
            (> dr 400)
            (< dr 600) )
           (then 
            (set-this 'durcontddr
              (infix (-7.0 / 200.0 * (600.0 - dr))) ))) )
     )))) 

(defun duration-contrast1-prop ()
  (let ((ndrmean))
    (each-track
      (each-note
        (if (first?) (setq ndrmean (ndrmean)))
        (then
          (let ((ddr% (1+  (* 0.05 (/ (- (this 'dr) ndrmean)
                                      ndrmean)))))
            (set-this 'durcontddr (- (* (this 'dr) ddr%) (this 'dr)))
            ))))))



(defun duration-contrast-rem-short-between-long ()
  (each-note-if
    (not (first?))
    (not (last?))
    (< (this 'dr)(prev 'dr))
    (< (this 'dr)(next 'dr))
    (then
      (rem-this 'durcontddr)
      )))

;remove in the case of double duration
(defun duration-contrast-rem-double-duration ()
 (each-note-if
   (not (first?))
   (not (last?))
   (not (this 'rest))
   (not (prev 'rest))
   (not (next 'rest))
   (< (this 'dr) 1000.)
   (= (round (* 2. (this 'ndr))) ;half as long ?
      (round (prev 'ndr)) )
   (> (next 'ndr) (this 'ndr))     ;following longer ?
   (then 
        (rem-this 'durcontddr)
        (rem-prev 'durcontddr) )))

(defun duration-contrast-short-soft (quant)
  (each-note-if
     (this 'durcontddr)
     (then
        (add-this 'sl (* quant 0.075 (this 'durcontddr))) )))

(defun duration-contrast-short-short (quant)
  (each-note-if
     (this 'durcontddr)
     (then
        (add-this 'dr (* quant 1.5 (this 'durcontddr))) )))


(defun duration-contrast-short-short-art (quant)
  (let ((out-of-legato t) (df0 1)) ;; df0=1 is an initialisation value
    (each-note
     (if (this 'legato-end)
         (setf out-of-legato t))
     (if (this 'legato-start)
         (setf out-of-legato nil))
     (if (this 'staccato)
         (setf out-of-legato nil))
     
     
     ;;;;;;;NUOVO!!!
     (ifn (or (last?)
           (next 'rest)
           (this 'rest)
           (this 'tie)
           (this 'dro) )  ;not if f ex phrases
          
          (setq df0 (abs (- (next-f0) (this-f0)))))
     ;;;;;;;; 
     (ifn (zerop df0)
     
     (when (and out-of-legato (this 'durcontddr))
       (if (this 'dro)
           (add-this 'dro (- (* quant 4.5 (this 'durcontddr))))
         (set-this 'dro (- (* quant 4.5 (this 'durcontddr)))))
       )))))




#| 
;boer endast verka i oevergaang mellan laang/kort resp kort/laang
;sharpening of durational contrast--------
;normal for amount = 1
;for 195 < dr <= 490 ==> dr = dr -3*quant
;for 135 < dr <= 195 ==> dr = dr -5*quant
;for       dr <= 135 ==> dr = dr -2*quant
(defun sharp-dur-cont (quant)
  (each-note
    (let ((dr (this 'dr)))
     (cond ((and (<= dr 490.)(> dr 195.))
            (add-this 'dr (* -3. quant)) )
           ((and (<= dr 195.)(> dr 135.))
            (add-this 'dr (* -5. quant)) )
           ((and (<= dr 135.))
            (add-this 'dr (* -2. quant)) )))))
|#
    

;;-------- Phrasing----------------------------------

;for phrase = t ==> dr = dr + 40*quant and dro = 80*quant
;for subph  = t ==> dro = 80*quant
;for last note  ==> dr = dr + 80*quant
(defun phrase (quant)
 (each-note-if
   (not (first?))
     (then
   (cond ((this 'phrase)        ;phrase
          (add-prev 'dr (* 40 quant)) 
          (set-prev 'dro (round (* 80 quant))) )
         ((last?)               ;last note
          (add-this 'dr  (* 80 quant) ))
         ((this 'subph)         ;subphrase
          (set-prev 'dro (round (* 80 quant))) )))))

;new variant using the phrase-start and phrase-end markers
(defun phrase-articulation (quant &key (phlevel 5)(subphlevel 6)(dur 1)(duroff 1))
   (if (not (check-for-phrase-marks))
      (print-ll "Phrase-articulation : no phrase marks - skipping rule")
      (each-note-if
       (not (first?))
       (then
        (cond ((last?)               ;last note
               (add-this 'dr  (* 80 dur quant) ))
              ((and (this 'phrase-end)
                    (member phlevel (this 'phrase-end)) )        ;phrase
               (add-this 'dr (* 40 dur quant))
               (if (this 'dro)
                  (add-this 'dro (round (* 80 duroff quant)))
                  (set-this 'dro (round (* 80 duroff quant))) ))
              ((and (this 'phrase-end)
                    (member subphlevel (this 'phrase-end)) )        ;subphrase
               (if (this 'dro)
                  (add-this 'dro (round (* 80 duroff quant)))
                  (set-this 'dro (round (* 80 duroff quant))) ))
              )))))

;;lengthen the second last note instead
(defun phrase-2last (quant)
 (each-note
   (cond ((next 'phrase)        ;phrase
          (add-prev 'dr (* 40 quant)) 
          (set-this 'dro (round (* 80 quant))) )
         ((last?)               ;last note
          (add-prev 'dr  (* 80 quant) ))
         ((next 'subph)         ;subphrase
          (add-prev 'dr (* 40 quant)) 
          (set-this 'dro (round (* 80 quant))) ))))

#|
;normal: (phrase-sf 40 80)
(defun phrase-sf (delay pause)
 (each-note
   (cond ((next 'phrase)        ;phrase
          (add-this 'dr delay) 
          (set-this 'dro (round pause)) )
         ((last?)                 ;last note
          (add-this 'dr  (* delay 2) ))
         ((next 'subph)         ;subphrase
          (set-this 'dro (round pause)) ))))
|#

;the "inverse" to phrase-sf 
;insert the same number of micro pauses randomly distributed
(defun phrase-inv (dro)
 (let ((nr-phrases 0))
 (each-note                        ;count phrases and subphrases
   (if (or (this 'phrase)
           (this 'subph) )
       (incf nr-phrases) ))
 (let ((*v* (eval (car *vlist*)))
       (*i*) )
  (loop for i from 1 to nr-phrases do
    (setq *i* (random 0 (length *v*)))
    (while (or (this 'dro)   ;not on these
               (this 'phrase)
               (this 'subphrase)
               (this 'rest)
               (next  'rest) )
           (setq *i* (random 0 (length *v*))) )
    (set-this 'dro dro) ))))
;----------------------
#|
(defun phrase (mulmax pausems)
   (phrase-u mulmax)
   (phrase-pause pausems) )

;phrasing with a power function perturbating dr
;over the whole phrase          
(defun phrase-u (mulmax)
 (each-note        
   (if (this 'phrase)
       (set-this 'subph t) ))
 (each-note
   (if (this 'subph)
       (hang-mul *i* (i?next-or-last *i* 'subph) mulmax 'dr) )
   ;set the last note
   (if (last?)
       (set-this 'dr (* mulmax (this 'dr))) )
   (if (and (this 'phrase)  ;clean up
            (this 'subph) )
       (irem *i* 'subph) )))

(defun phrase-pause (pause)
 (each-note
   (cond ((or (next 'phrase)
               (next 'subph) )
          (set-this 'dro pause) ))))
        
;set the tones related to the durations to get a xxx
;that is added to the old values
;NOT included the last one
(defun hang-mul (i-from i-to mulmax prop)
  (let ((drdist (drsum i-from i-to)))
       (loop for i from i-from to (1- i-to) do
         (ifn (iget i 'rest)
          (let* ((drsum (drsum i-from i))
                 ;drnorm is between -1 - +1
                 (drnorm (+ -1. (* 2. (/ drsum drdist)))) )
           (iset i prop (* (iget i prop)
                      (+ 1 (* (- mulmax 1.)
                              ;using a quadratic function
                              (power drnorm 2) )))
                    ) )))))
|#

;;-------- Reduction of durational contrast -------------

;should only trigger on weak beats ? /af
;;this rule cannot be increased above quant = 1
;; since that will destroy the time values
; same selection in durational contrast
;; ratio will be about 1.68 (2-0.12)/(1+0.12) for k=1
(defun Double-duration (quant)
 ;(if (> quant 1) (setq quant 1)) ;limit of quant = 1
 (each-note-if        ;  40.40
   (not (first?))
   (not (last?))
   (not (this 'rest))
   (not (prev 'rest))
   (not (next 'rest))
   (< (this 'dr) 1000.)
   (= (round (* 2. (this 'ndr))) ;half as long ?
      (round (prev 'ndr)) )
   (> (next 'ndr) (this 'ndr))     ;following longer ?
   (then 
      (let ((ddr (* 0.12 quant (this 'ndr))))
        (set-this 'dr (+ (this 'dr) ddr))
        (set-prev 'dr (- (prev 'dr) ddr)) ))) )

;;same as above for 3:1 nominal duration
(defun triple-duration (quant)
 (each-note-if        
   (not (first?))
   (not (last?))
   (not (this 'rest))
   (not (prev 'rest))
   (not (next 'rest))
   (< (this 'dr) 1000.)
   (= (round (* 3. (this 'ndr))) ;three times as long ?
      (round (prev 'ndr)) )
   (> (next 'ndr) (this 'ndr))     ;following longer ?
   (then 
      (let ((ddr (* 0.3 quant (this 'ndr))))
        (set-this 'dr (+ (this 'dr) ddr))
        (set-prev 'dr (- (prev 'dr) ddr)) ))) )

;;special case for a note followed by a triplet of the same duration
;;used for the mozart piece
;;this note second of triplets
;;k=1 make first note 20 percent longer
;;2002-11/AF
(defun note-triplet-contrast (quant)
 (each-note-if        
  (not (first?))
  (not (first+1?))
  (not (last?))
  (not (last-1?))
  (not (this 'rest))
  (not (prev 'rest))
  (not (prev2 'rest))
  (not (next 'rest))
  (not (next2 'rest))
  (< (this 'ndr) 300.0)   ;only short triplets
  (= (round (this 'ndr))  ;same nominal duration in triplet 
     (round (prev 'ndr))
     (round (next 'ndr)))
  (= (round (prev2 'ndr))          ;total nom triplet dur same as preceeding note
     (round (+ (prev 'ndr)(this 'ndr)(next 'ndr))) )
  (> (next2 'ndr) (next 'ndr))     ;following longer ?
  (then 
      (let ((ddr (* 0.2 quant (prev2 'ndr)))) 
        (set-prev2 'dr (+ (prev2 'dr) ddr))
        (set-prev 'dr (- (prev 'dr) (/ ddr 3.0)))
        (set-this 'dr (- (this 'dr) (/ ddr 3.0)))
        (set-next 'dr (- (next 'dr) (/ ddr 3.0)))
        ))))

(defun social-duration-care (quant)
 (each-note-if        ;  40.50
   (not (first?))
   (not (last?))
   (not (this 'rest))
   ;(not (prev 'rest)) ;removed 20011106/af
   (not (next 'rest))
   (< (this 'dr) (next 'dr))
   (< (this 'dr) (prev 'dr))
   (< (this 'dr) 100)
   (then
    (let ((ddr (* quant 10.)))
       (set-this 'dr (+ (this 'dr) ddr))
       (set-prev 'dr (- (prev 'dr) ddr)) ))) 
)
 
   

;;-------- Lengthening of second note in singular leaps--------
 
(defun leap-tone-duration (amount)
  (mark-leap)
  (leap-tone-duration-rise amount)
  (leap-tone-duration-fall amount)
  (each-note                ;clean up
    (rem-this 'leap) ))

(defun mark-leap ()
  (each-note-if
    (not (last?))
    (not (this 'rest))
    (not (next 'rest))
    (then
      ;(if (next 'leap) (setq *i* (i?last)))  ;exit if already marked
      (set-this 'leap 
        (- (next-f0) (this-f0)) )
         )))

;*fixa för mycket just nu kolla billy test
;target note: dr = dr + 4.2*quant*(sqrt leap) 
;start  note: dr = dr - 4.2*quant*(sqrt leap) 
(defun leap-tone-duration-rise (quant)
 (each-note-if
  (not (this 'phrase))
  (not (this 'subph))
  (not (this 'motive))
  (not (this 'phrase-start))
  (not (first?))
  (not (last?))
  (prev 'leap)
  (> (prev 'leap) 2)        ;starting at a minor third
  (not (and                 ;not for up-down-up-leap pattern
            (this 'leap)
            (next 'leap)
            (< (this 'leap) -2)
            (> (next 'leap) 2) ))
  (then
     (let ((addnr (* 4.2 quant (sqrt (prev 'leap)))))
      ;    (setq addnr (* addnr (/ (this 'dr) 320))) ;prop to dr
          (set-this 'dr                        ;target note
                (+ (this 'dr) addnr) ) 
          (set-prev 'dr                        ;preceeding note
                (- (prev 'dr) addnr)) ))))

;target note: dr = dr - 4.2*quant*(sqrt (abs leap)) 
;start  note: dr = dr + 2.4*quant*(sqrt (abs leap)) 
(defun leap-tone-duration-fall (quant)
 (each-note-if
  (not (first?))
  (not (this 'phrase))
  (not (this 'subph))
  (not (this 'motive))
  (not (this 'phrase-start))
  (not (first?))
  (not (last?))
  (prev 'leap)
  (< (prev 'leap) -2)        ;starting at a minor third
  (not (and                 ;not for down-up-down-leap pattern
            (this 'leap)
            (next 'leap)
            (> (this 'leap) 2)
            (< (next 'leap) -2) ))
  (then
     (let ((addnr (* quant (sqrt (abs (prev 'leap))))))
       ;   (setq addnr (* addnr (/ (prev 'dr) 320))) ;prop to dr
          (set-this 'dr                        ;target note
                (- (this 'dr) (* 4.2 addnr)) ) 
          (set-prev 'dr                        ;preceeding note
                (+ (prev 'dr) (* 2.4 addnr)) )))))


;;-------- Raising tempo in uphill motion ------------
 
;dr=dr-2 if next note is higher and previous is lower
(defun faster-uphill (quant)
 (each-note-if
   (not (last?))
   (not (first?))
   (not (this 'rest))
   (not (next 'rest))
   (not (prev 'rest))
   (> (next-f0) (this-f0))
   (> (this-f0) (prev-f0))
   (then
      (set-this 'dr (- (this 'dr) (* quant 2)))
      (set-this 'upflag t)
      (ifn (prev 'upflag)
         (set-prev 'dr (- (prev 'dr) (* quant 2))) )
        ))
  (each-note (rem-this 'upflag)) )


;;---------------- Micropause in leaps ----------------

;---needs work
;only if first note's dr > 100 ms
; delta-dr = 8 * |delta-f0| * quant    for    2 < |delta-f0| <= 9
;; ddr = ddr * 0.3/600(dr - 100) + 0.3  dduration dependence
(defun leap-articulation-dro (quant)
  (each-note-if
      (not (last?))
      (not (next 'rest))
      (not (this 'rest))
      (not (this 's))
      (not (this 'dro))         ;not if f ex phrases
      (> (this 'dr) 100)         ;not for short notes
    (then
     (let ((df0 (abs (- (next-f0) (this-f0))))
           (dr (this 'dr)) )
     (if (> df0 9) (setq df0 9))   ;maj sixth is max
     (if (> df0 2)
         (set-this 'dro 
            (round (* 8 df0 quant 
                     (infix (0.3 / 600. * (dr - 100) + 0.3)) ))) ;dur dependence
         )))))

(defun leap-articulation-dro-inv (dro)
(let ((df0) (nr-leaps 0))
 (each-note                        ;count number of leaps 
  (ifn (or (next 'rest)  ;**** check if next row is correct
                    (this 'rest)
           (last?) 
           (this 'dro) )  ;not if f ex phrases
   (if (> (this 'dr) 250)         ;not for short notes
    (progn
     (setq df0 (abs (- (next-f0) (this-f0))))
     (if (> df0 9) (setq df0 9))   ;sext is max
     (cond ((> df0 2)
            (incf nr-leaps)
            (set-this 'leap t) ))))))
 (let ((*v* (eval (car *vlist*)))
       (*i*) )
  (loop for i from 1 to nr-leaps do
    (setq *i* (random 0 (length *v*)))
    (while (or (this 'dro)   ;not on these
               (this 'leap)
               (this 'rest)
               (next  'rest) )
           (setq *i* (random 0 (length *v*))) )
    (set-this 'dro  dro) ))
 (each-note                        ;cleaning
    (if (this 'leap)
        (rem-this 'leap) ))
 ))


;;-------- Note repetition --------------------  

;the repetition of the same note

#|
(defun repetition-articulation-dro (quant)
  (let (df0)
    (each-note
  (ifn (or (last?)
           (next 'rest)
           (this 'rest)
           (this 'tie)
           (this 'dro) )  ;not if f ex phrases
    (progn
     (setq df0 (abs (- (next-f0) (this-f0))))
      (cond ((zerop df0)
             (set-this 'dro (round (*  quant 35.)))
             )))))))
|#

(defun repetition-articulation-dro (quant)
   (each-note-if
    (not (last?))
    (not (next 'rest))
    (not (this 'rest))
    (not (this 'tie))
    (= (next-f0) (this-f0))
    (then
     (set-this 'dro (round (*  quant 35.)))
    )))

;;-------- Rem dro of repeteated notes --------------------  

;the repetition of the same note

(defun rem-repetition-articulation-dro ()
  (let (df0)
    (each-note
  (ifn (or (last?)
           (next 'rest)
           (this 'rest)
           (this 'tie)
           (this 'dro) )  ;not if f ex phrases
    (progn
     (setq df0 (abs (- (next-f0) (this-f0))))
      (cond ((zerop df0)
             (rem-this 'dro)
             )))))))

;-------  

;endast vid flerstämmighet
;used for low organ voices
;dro = k*quant*log(dr)/f0
(defun low-short (quant)
 (let ((k 2.4)(upper-limit 60))
  (each-note-if
    (not (this 'rest))
    (< (this-f0) upper-limit)
    (then
     (if (this 'dro)
     (add-this 'dro
        (* k quant (log (this 'dr) 10) (- upper-limit (this 'f0))) )
     (set-this 'dro
        (* k quant (log (this 'dr) 10) (- upper-limit (this 'f0))) )
      )))))




;end

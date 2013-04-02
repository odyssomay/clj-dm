
;;Lindströms mus tension - work in progress
;;use mark-metric-level from violinvibrato
;;2001-10-16/af

(in-package :dm)

;;mark :metric-level :kk-profile :pivot :phrase-peak
(defun mark-features ()
  (mark-melodic-charge)
  (mark-krumhansl-key-profile)
  (MARK-METRIC-LEVEL)
  (mark-pivot-note)
  (MARK-TOP-NOTE-IN-PHRASE)
  (MARK-MUSICAL-TENSION)
  )

(defun mark-musical-tension ()
  (each-note-if
   (this :kk-profile)
   (this :METRIC-LEVEL)
   (not (this 'rest))
   (then
    (set-this :tension (+ 4.064595
                        (* 0.000035 (this 'dr))
                          (- (* 0.055312 (this :kk-profile)))
                          (* 0.010236 (this :metric-level)) ))
    (set-this :tension (* (- (this :tension) 3.70) 3)) ;approx. normalizing
    )))

(defun musical-tension (q)
  (mark-krumhansl-key-profile)
  (MARK-METRIC-LEVEL)
  (mark-musical-tension)
  (each-note-if 
   (this :tension)
   (then
    (add-this 'sl (* q (this :tension) 6))
    ))
  (rem-all :kk-profile)
  (rem-all :METRIC-LEVEL)
  (rem-all :tension)
  )

   

(defun krumhansl-major-key-profile-fn (nr)
    (case nr
      (0 6.35) (1 2.23) (2 3.48) (3 2.33) (4 4.38)
      (5 4.09) (6 2.52) (7 5.19) (8 2.39) (9 3.66) (10 2.29) (11 2.88)
       ))  
(defun krumhansl-minor-key-profile-fn (nr)
    (case nr
      (0 6.33) (1 2.68) (2 3.52) (3 5.38) (4 2.60)
      (5 3.53) (6 2.54) (7 4.75) (8 3.98) (9 2.69) (10 3.34) (11 3.17)
       ))  

;mark Krumhansl values in the prop :kk-profile
(defun mark-krumhansl-key-profile ()
 (block mark
 (let ((qnr)(minor? nil))
  (each-note 
   (if (and (first?) (not (this 'key)))(return-from mark))
   (if (this 'key) (setq qnr (tone-to-tonnr (this 'key))) )
   (if (and (this 'modus)(equal (this 'modus) "min")) (setq minor? t))   
   (cond 
    ((and (not (this 'rest)) minor?)   
    (set-this :kk-profile
        (krumhansl-minor-key-profile-fn
             (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12) )))
    ((and (not (this 'rest)) (not minor?))   
    (set-this :kk-profile
        (krumhansl-major-key-profile-fn
         (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12) )))
    )))))


(defun mark-pivot-note ()
  (each-note-if
   (not (first?))
   (not (this 'phrase-start))
   (not (this 'phrase-end))
   (not (or (this 'rest)(prev 'rest)(next 'rest)))
   (then
    (if (and (> (this-f0)(prev-f0))
             (> (this-f0)(next-f0)) )
        (set-this :pivot t)
      ))))

(defun mark-top-note-in-phrase ()
  (each-note-if
   (this 'phrase-start)
   (then
    (let ((iend (i?next *i* 'phrase-end))
          (maxf0 0)
          (maxi nil))
      (loop for i from *i* to iend do
            (when (and (iget i 'f0) (>= (iget i 'f0) maxf0))
              (setq maxf0 (iget i 'f0))
              (setq maxi i) ))
      (iset maxi :phrase-peak t)
      ))))
      
;;;-------------- utilities --------------------
;;used for eriks melodies
(defun mark-phrase-from-bar ()
  (each-note
   (then
    (if (and (this 'bar)(not (last?))) (set-this 'phrase-start '(6)))
    (if (and (this 'bar)(not (first?))) (set-prev 'phrase-end '(6)))
    (if (last?) (set-this 'phrase-end '(6)))
    )))




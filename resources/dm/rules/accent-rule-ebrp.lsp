;; Bisesi/Parncutt accent rules

;;Apply a curve in the sound level and/or the duration in the surrounding of an accent.
;;The music must contain boundary marks in the following way:
;; On the note corresponding to an accent:
;;    left <number of notes involved before the main event (start counting from 0)>
;;    right <number of notes involved after the main event (start counting from 0)>
;;    peak <delta deviation in the sound level or in the duration>
;;    left-fn <function used to model the pattern before the main event>
;;    right-fn <function used to model the pattern after the main event>

;;1002 af/eb : preliminary version
;;101210/af/eb New definitions, fixing more curves, additive but only sound level
;;adapted to graphical interface as defined in accentdialog-win
;;101213/af added accent-dr, integrated them in the rule system with support for polyphony, 
;; multiple rules at the same time
;; a general quantity parameter
;;111111/af new version again wih individual rules for each accent
;;120313/af adjusted accent quantities
;;120314/af added width scaling
;;120417/af added dynamic-accent


(in-package :dm)


;;;---- new version with individual rules for each accent ------
;; each rules expect a (for now) manual annotation of an accent mark (accent-c, accent-h, accent-m)
;; together with a salience number (from 1-5)
;; quant  the general quantity of the rule, default 1
;; curve  :linear :linear :quadratic :cubic :exponential :cosine :gaussian :hand-gesture
;; amp    scaling parameter for sound level, default 1 (no scaling)
;; dur    scaling parameter for duration (IOI), default 1 (no scaling)
;; width  scaling parameter for width, default 1 (no scaling)

;new accent only for dynamic variation
;tempo variation possible if the :dur keyword is used with a value greater than 0
;tag in the score: accent-d
(defun dynamic-accent (quant &key (curve :linear) (amp 1) (dur 0) (width 1))
  (each-note-if
   (this 'accent-d)
   (then
    (let* ((sal (this 'accent-d))
           (sal2 (+ (* 0.625 sal) 1.875))
           (w1 (* width 250.0 sal2))
           (w2 (* width 250.0 sal2))
           ;(quant-sl (* quant amp 1 sal))
           ;(quant-dr (* quant dur 0.25 0.5 0.2 sal))
           (quant-sl (* quant amp 2 sal2))
           (quant-dr (* quant dur 0.05 sal2))
          )
     ;(set-this 'accent-sl (list w1 w2 quant-sl  curve curve))
      ;(set-this 'accent-dr (list w1 w2 quant-dr  curve curve))
      (accent-apply-sl *i* w1 w2 quant-sl curve curve)
      (accent-apply-dr *i* w1 w2 quant-dr curve curve)
      ))))

(defun melodic-contour-accent (quant &key (curve :linear) (amp 1) (dur 1) (width 1))
  (each-note-if
   (this 'accent-c)
   (then
    (let* ((sal (this 'accent-c))
           (sal2 (+ (* 0.625 sal) 1.875))
           (w1 (* width 250.0 sal2))
           (w2 (* width 250.0 sal2))
           ;(quant-sl (* quant amp 1 sal))
           ;(quant-dr (* quant dur 0.25 0.5 0.2 sal))
           (quant-sl (* quant amp 2 sal2))
           (quant-dr (* quant dur 0.05 sal2))
          )
     ;(set-this 'accent-sl (list w1 w2 quant-sl  curve curve))
      ;(set-this 'accent-dr (list w1 w2 quant-dr  curve curve))
      (accent-apply-sl *i* w1 w2 quant-sl curve curve)
      (accent-apply-dr *i* w1 w2 quant-dr curve curve)
      ))))

(defun harmonic-accent (quant &key (curve :linear) (amp 1) (dur 1) (width 1))
  (each-note-if
   (this 'accent-h)
   (then
    (let* ((sal (this 'accent-h))
           (sal2 (+ (* 0.625 sal) 1.875))
           (w1 (* width 250.0 sal2))
           (w2 (* width 250.0 sal2))
           ;(quant-sl (* quant amp 1 sal))
           ;(quant-dr (* quant dur 0.25 0.5 0.2 sal))
           (quant-sl (* quant amp 2 sal2))
           (quant-dr (* quant dur 0.05 sal2))
          )
     ;(set-this 'accent-sl (list w1 w2 quant-sl  curve curve))
      ;(set-this 'accent-dr (list w1 w2 quant-dr  curve curve))
      (accent-apply-sl *i* w1 w2 quant-sl curve curve)
      (accent-apply-dr *i* w1 w2 quant-dr curve curve)
      ))))

(defun metrical-accent (quant &key (curve :linear) (amp 1) (dur 1) (width 1) (marker 'accent-m))
  (each-note-if
   (this marker)
   (then
      ;(print-ll "i = " *i*)
    (let* ((sal (this marker))
           (sal2 (+ (* 0.625 sal) 1.875))
           (w1 (* width 250.0 sal2))
           (w2 (* width 250.0 sal2))
           ;(quant-sl (* quant amp 1 sal))
           ;(quant-dr (* quant dur 0.25 0.5 0.2 sal))
           (quant-sl (* quant amp 2 sal2))
           (quant-dr (* quant dur 0.05 sal2))
          )
     ;(set-this 'accent-sl (list w1 w2 quant-sl  curve curve))
      ;(set-this 'accent-dr (list w1 w2 quant-dr  curve curve))
      (accent-apply-sl *i* w1 w2 quant-sl curve curve)
      (accent-apply-dr *i* w1 w2 quant-dr curve curve)
      ))))
#|
;; Applied the sound level variations on the notes
;; first make 'dsl and then add that to 'sl
;; with added floats marking time in ms
;; fractional time not yet implemented
(defun accent-apply-sl (inote ext-left ext-right peak curve-left curve-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- note-number ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ note-number ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'dsl power-left fun-left fun-left)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'dsl power-right fun-right fun-right)
    ;transfer dsl to sl
    (loop for i from istart to iend do
          (if (iget i 'sl) (iadd i  'sl (iget i 'dsl)))
          (rem-var (nth i *v*) 'dsl)
          )))
|#
;; with max instead of add so that envelopes don't add up
;; not really compatible with the traditional rule application but works well with the new rule interaction stuff
;; will probably not work with negative values as well
;; 120327/af
;; 130114/af included dsl (new score dynamics)
(defun accent-apply-sl (inote ext-left ext-right peak curve-left curve-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- note-number ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ note-number ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    ;(print-ll "istart " istart " inote " inote " iend " iend)
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'dsl power-left fun-left fun-left)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'dsl power-right fun-right fun-right)
    ;transfer dsl to sl
    (loop for i from istart to iend do
          (if (iget i 'sl) 
              (iset i  'sl (max (iget i 'sl) (+ (iget i 'nsl)(iget i 'dsl)))) )
          (rem-var (nth i *v*) 'dsl)
          )))

#|
;; same for dr
(defun accent-apply-dr (inote ext-left ext-right peak curve-left curve-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- note-number ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ note-number ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'ddr power-left fun-left fun-left)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'ddr power-right fun-right fun-right)
    (loop for i from istart to iend do
        (iset i  'dr (* (iget i 'dr) (1+ (iget i 'ddr))))
        (rem-var (nth i *v*) 'ddr)
          )))
|#

;; with max instead of add so that envelopes don't add up
;; not really compatible with the traditional rule application but works well with the new rule interaction stuff
;; 120327/af
(defun accent-apply-dr (inote ext-left ext-right peak curve-left curve-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- note-number ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ note-number ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    ;(print-ll " istart " istart " inote " inote)
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'ddr power-left fun-left fun-left)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'ddr power-right fun-right fun-right)
    (loop for i from istart to iend do
        (iset i  'dr (max (iget i 'dr) (* (iget i 'ndr) (1+ (iget i 'ddr)))))
        (rem-var (nth i *v*) 'ddr)
        )))


;;;;------------------------------------------------------------------------

#|
;;; main rule function
(defun accent-main (quant)
  (rem-all 'accent-sl)
  (rem-all 'accent-dr)
  (accent-translate-marks quant)
  (accent-main-sl quant)
  (accent-main-dr quant)
  )

;; work to do:
;; 1. adjust individual levels and widths for each kind of mark (salience 1-5)
;; 2. adjust the combination
(defun accent-translate-marks (quant)
  ;(print "hej")
  (each-note-if
   (or (this 'accent-c)(this 'accent-h)(this 'accent-m) )
   (then
   ;(print-ll " accent c " (this 'accent-c) " accent h " (this 'accent-h) " accent m " (this 'accent-m))
   (let ((w1 0.0)(w2 0.0)
         (sal 0.0)
         (sal-c (or (this 'accent-c) 0.0)) (sal-h (or (this 'accent-h) 0.0)) (sal-m (or (this 'accent-m) 0.0)) )
     (setq sal (sqrt (+ (expt sal-c 2) (expt sal-h 2) (expt sal-m 2))))  ;weight the combination of several
     ;(print-ll " sal c " sal-c " sal-h " sal-h " sal m " sal-m )
     (setq w1 (* 250.0 sal))
     (setq w2 (* 250.0 sal))
     (set-this 'accent-sl (list w1 w2 (* 0.25 sal)  :linear :linear))
     (set-this 'accent-dr (list w1 w2 (* 0.5 0.25 sal)  :linear :linear))
     ))))
     

;;;----- apply sound level accents from specific accent marks in the score ---------

;;apply all marked accents in the score
;; default 4 dB for quant or peak = 1
(defun accent-main-sl (quant)
  (each-note-if
   (this 'accent-sl)
   (then
    (let ((left (first (this 'accent-sl)))
          (right (second (this 'accent-sl)))
          (peak (third (this 'accent-sl)))
          (fun-left (fourth (this 'accent-sl)))
          (fun-right (fifth (this 'accent-sl)))
          function-left function-right power-left power-right)
      (multiple-value-setq (function-left power-left) (accents-translate-curv-name-left fun-left))
      (multiple-value-setq  (function-right power-right) (accents-translate-curv-name-right fun-right))
      (apply-accent-sl *i* left right (* quant peak 4.0) function-left function-right power-left power-right)
      ))))

   
;; Applied the sound level variations on the notes
;; first make 'dsl and then add that to 'sl
(defun apply-accent-sl (note-number ext-left ext-right peak fun-left fun-right power-left power-right)
  (iset-ramp-x2-decimal-last (- note-number ext-left) note-number 0.0 0.0 0.0 peak 'dsl power-left fun-left fun-left)
  (iset-ramp-x2-decimal-last note-number (+ note-number ext-right) 0.0 0.0 peak 0.0 'dsl power-right fun-right fun-right)
  (loop for i from (- note-number ext-left) to (+ note-number ext-right) do
        (if (iget i 'sl) (iadd i  'sl (iget i 'dsl)))
        (rem-var (nth i *v*) 'dsl)
        ))

;; with added floats marking time in ms
;; fractional time not yet implemented
(defun apply-accent-sl (inote ext-left ext-right peak fun-left fun-right power-left power-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- note-number ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ note-number ext-left) (i?last)) ))
        )
  ;(print-ll istart " " inote " " iend)
  (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'dsl power-left fun-left fun-left)
  (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'dsl power-right fun-right fun-right)
  (loop for i from istart to iend do
        (if (iget i 'sl) (iadd i  'sl (iget i 'dsl)))
        (rem-var (nth i *v*) 'dsl)
        )))

;;;----- apply ioi (duration) accents from specific accent marks in the score ---------

;;apply all marked accents in the score for dr
;; default 20% for quant or peak = 1
(defun accent-main-dr (quant)
  (each-note-if
   (this 'accent-dr)
   (then
    (let ((left (first (this 'accent-dr)))
          (right (second (this 'accent-dr)))
          (peak (third (this 'accent-dr)))
          (fun-left (fourth (this 'accent-dr)))
          (fun-right (fifth (this 'accent-dr)))
          function-left function-right power-left power-right)
      (multiple-value-setq (function-left power-left) (accents-translate-curv-name-left fun-left))
      (multiple-value-setq  (function-right power-right) (accents-translate-curv-name-right fun-right))
      (apply-accent-dr *i* left right (* quant peak 0.2) function-left function-right power-left power-right)
      ))))

;; Apply the IOI variations on the notes
;; first make 'ddr and then multiply that to 'dr
(defun apply-accent-dr (note-number ext-left ext-right peak fun-left fun-right power-left power-right)
  (iset-ramp-x2-decimal-last (- note-number ext-left) note-number 0.0 0.0 0.0 peak 'ddr power-left fun-left fun-left)
  (iset-ramp-x2-decimal-last note-number (+ note-number ext-right) 0.0 0.0 peak 0.0 'ddr power-right fun-right fun-right)
  (loop for i from (- note-number ext-left) to (+ note-number ext-right) do
         (iset i  'dr (* (iget i 'dr) (1+ (iget i 'ddr))))
        (rem-var (nth i *v*) 'ddr)
        ))

;; with floats marking time in ms
;; fractional time not yet implemented
(defun apply-accent-dr (inote ext-left ext-right peak fun-left fun-right power-left power-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- note-number ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ note-number ext-left) (i?last)) ))
        )
  ;(print-ll istart " " inote " " iend)
  (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'ddr power-left fun-left fun-left)
  (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'ddr power-right fun-right fun-right)
  (loop for i from istart to iend do
        (iset i  'dr (* (iget i 'dr) (1+ (iget i 'ddr))))
        (rem-var (nth i *v*) 'ddr)
        )))

|#

;;----utility functions---------------

;;translates the names from the drop down menu to function names and power values
(defun accents-translate-curv-name-right (keyword)
  (let (function (power 0))
    (when (equal :linear keyword) (setq function 'power-fn-acc) (setq power 1))
    (when (equal :quadratic keyword) (setq function 'power-fn-acc) (setq power 2))
    (when (equal :cubic keyword) (setq function 'power-fn-acc) (setq power 3))
    (when (equal :exponential keyword) (setq function 'accum-fn-acc) (setq power 1))
    (when (equal :cosine keyword) (setq function 'cos-fn-acc))
    (when (equal :gaussian keyword) (setq function 'gauss-fn-acc))
    (when (equal :hand-gesture keyword) (setq function 'hand-gesture-fn-acc))
    (values function power)
    ))

(defun accents-translate-curv-name-left (keyword)
  (let (function (power 0))
    (when (equal :linear keyword) (setq function 'power-fn-dec) (setq power 1))
    (when (equal :quadratic keyword) (setq function 'power-fn-dec) (setq power 2))
    (when (equal :cubic keyword) (setq function 'power-fn-dec) (setq power 3))
    (when (equal :exponential keyword) (setq function 'accum-fn-dec) (setq power 1))
    (when (equal :cosine keyword) (setq function 'cos-fn-dec))
    (when (equal :gaussian keyword) (setq function 'gauss-fn-dec))
    (when (equal :hand-gesture keyword) (setq function 'hand-gesture-fn-dec))
    (values function power)
    ))

;; get index ndrtarget ms before the note iend
;; returns index and remaining ndr
;; if it reach the beginning it returns the start pos (0 0)
(defun i?ndr-before-index (ipos ndrtarget)
  (let ((i ipos) (ndrtot 0))
    (while (and (< ndrtot ndrtarget) (> i 0))
      (decf i)
      (incf ndrtot (iget i 'ndr)) )
      (values i (if (<= (- ndrtot ndrtarget) 0) 0 (- ndrtot ndrtarget)))
    ))

;;same after
;; if it reach the end it returns the last pos (<last index> <ndr last note>)
(defun i?ndr-after-index (ipos ndrtarget)
  (let ((i ipos) (ndrtot 0) (ilast (i?last)))
    (while (and (< (+ ndrtot (iget i 'ndr)) ndrtarget) (< i ilast))
      (incf ndrtot (iget i 'ndr))
      (incf i)
      )
    (values i (if (= i ilast) 
                  (min (iget i 'ndr) (- ndrtarget ndrtot))
                (- ndrtarget ndrtot)
                ))
    ))


;;FUNCTIONS used for the shape of accents:
;;----power function
(defun power-fn-dec (x power)
  (expt x power))
(defun power-fn-acc (x power)
  (abs (expt (- x 1) power)))


;;----gaussian
(defun gauss-fn-dec (x power)
  (exp (/ (expt (- x 1) 2) -0.1))
  )
(defun gauss-fn-acc (x power)
  (gauss-fn-dec (- 1 x) 0)
  )     

;;----cosinus
(defun cos-fn-dec (x power)
  (/ (+ (cos (- (* x pi) pi)) 1) 2.0)
  )
(defun cos-fn-acc (x power)
  (cos-fn-dec (- 1 x) 0)
  ) 

;;----exp1 
(defun accum-fn-dec (x power)
  (exp (/ (expt (- x 1) 1) 0.1))
  )
(defun accum-fn-acc (x power)
  (accum-fn-dec (- 1 x) 0)
  )

;;----exp2
(defun accumslow-fn-dec (x power)
  (exp (/ (expt (- x 1) 1) 1.5))
  )
(defun accumslow-fn-acc (x power)
  (accumslow-fn-dec (- 1 x) 0)
  )

;;----exp3
(defun accumfast-fn-dec (x power)
  (exp (/ (expt (- x 1) 1) 0.08))
  )
(defun accumfast-fn-acc (x power)
  (accumfast-fn-dec (- 1 x) 0)
  )



;;----------------end----------------------------




    


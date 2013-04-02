
;; this is the new punctuation taken from Punctoptim3.lisp using the 970804 version /af970805
 
;;evaluation of punctuation rule with optimization of weight parameters
;; AF 9705
;;Punctoptim3 test with reduce in appoggiatura instead of remove /af9707
;; modification of mass rule to work with the back example /af970804
;;
;;971117/af converted to DM 2.0
;;000627/af punct-apply changed to exclude note before rest

(in-package :dm)
(export '())

(defvar *rule-debug-info*)   ;;fix should move to dm-objects??
(setq *rule-debug-info* nil)

(defun punctuation (quant &key (dur 1)(duroff 1)(markphlevel7 nil) (marker :pause))
  (mark-punctuation 2 -4 10 9 9 5 5 4 3 1.2 1 10.5 1.3 5 0.6 2.5 0.9 6 0.5)
  (cond
   ((equal marker :pause)
    (punct-apply quant :dur dur :duroff duroff) )
   ((equal marker :ampdip)
    (punct-apply-dip quant) )
   (t
    (error "unknown marker value: ~A (defined marker values:  :pause, :ampdip)" marker ) )
   )
  (if markphlevel7 (mark-phrase7-from-punct))
  (if (not *rule-debug-info*) (rem-all :punct))
  (if (not *rule-debug-info*) (rem-all :weight))
  (rem-all 'leap)
)

;make the punctuation according to the marks by lasse
(defun punctuation-lf (q)
  (each-note-if
    (this 'punct-lf)
    (then 
      (set-this :weight 5) ))
  (punct-apply q)
  (if (not *rule-debug-info*) (rem-all :weight))
  )

;make the punctuation according to the NN model
(defun punctuation-nn (q)
  (each-note-if
    (this 'punct-nn)
    (then 
      (set-this :weight 5) ))
  (punct-apply q)
  (if (not *rule-debug-info*) (rem-all :weight))
  )

;----- marking of phrase level 7 --------------

(defun mark-phrase7-from-punct ()
  (each-note
    (when (not (this 'rest))
      (iadd-phrase-start-level *i* 7)
      (exit-track) ))
  (each-note-if
    (this :weight)
    (then
      (iadd-phrase-end-level *i* 7)
      (let ((i (+ *i* 1)))
        (until (not (iget i 'rest)) (incf i))
        (iadd-phrase-start-level i 7) )
      ))
  (each-note-if
    (last?)
    (then
      (let ((i *i*))
        (until (not (iget i 'rest)) (decf i))
        (iadd-phrase-end-level i 7)
        ))) )
      

(defun iadd-phrase-start-level (i level)
  (if (and (iget i 'phrase-start) 
           (not (member level (iget i 'phrase-start))) )
    (iset i 'phrase-start (append (iget i 'phrase-start) (list level)))
    (iset i 'phrase-start (list level))
    ))

(defun iadd-phrase-end-level (i level)
  (if (and (iget i 'phrase-end) 
           (not (member level (iget i 'phrase-end))) )
    (iset i 'phrase-end (append (iget i 'phrase-end) (list level)))
    (iset i 'phrase-end (list level))
    ))

;-------utilities-----------

(defun print-lf-not-rule ()
  (each-track
    (print (get-first 'mel))
    (each-note-if
      (this 'punct-lf)
      (not (this :weight))
      (then
        (print-ll " bar " (this 'bar) " note " (this 'n) " punct-lf " (this 'punct-lf) " :punct " (this :punct))
        ))))

;(defun pm () (print-music-prop :weight :punct 'n 'dot 'q 'punct-lf))

;(defun play-seq ()
;  (each-voice
;    (print (get-first 'mel))
;    (playlist)) )


;the punctuation selected for uppsala97 optimized for the optim26melgroup
(defun final-punct-970605 (quant)
  (mark-punctuation 3 -30 10 9 9 5 5 4 3 1.2 1 10.5 1.3 5 0.7 3.5 1 6 0.5)
  ;(print-count)
  (punct-apply quant) )

;punctuation with reduce in appoggiatura
;selected for the final version and genova97
(defun final-punct-970804 (quant)
  (mark-punctuation 2 -4 10 9 9 5 5 4 3 1.2 1 10.5 1.3 5 0.6 2.5 0.9 6 0.5)
  ;(print-count)
  (punct-apply quant) )

(defun final-punct-970805 (quant)
  (mark-punctuation 1 -4 9 9 9 5 5 4 3 1.2 1 10.5 1.3 5 0.6 2.5 0.9 6 0.5)
  ;(print-count)
  (punct-apply quant) )

;-----------------------------------


#|
(defun scale-droff-poly ()
  (let ((*vlist* '(v1)))
    (print *vlist*)
    (scale-dro 1) )
  (let ((*vlist* '(v2)))
    (print *vlist*)
    (scale-dro 4) )
  (let ((*vlist* '(v3)))
    (print *vlist*)
    (scale-dro 4) )
  (let ((*vlist* '(v4)))
    (print *vlist*)
    (scale-dro 8) )
  )
|#



;;--------------- rule application ---------------------------

;;apply the punctuation with all weight parameters
;; original values
;(mark-punctuation 5 -100? 10 10 6 3 4 5 3 1 1 6.5 1 5 0.6 3.5 1 6 0.6))
(defun mark-punctuation (app reduce-in-app
                         mass1 mass2 mass3 mass4 mass5 mass6 mass7 mass-leap mass-dur
                         long long-dur long-first
                         short
                         fish fish-dur shbelo
                         mean-weight-factor)
  (each-track 
    (rem-all :weight)
    (rem-all :punct)
    (mark-leap)
    (punct-rest)
    (punct-appoggiatura :app app)
    (rem-in-stepmotion)
    (punct-mass :mass1 mass1 :mass2 mass2 :mass3 mass3 :mass4 mass4
                :mass5 mass5 :mass6 mass6 :mass7 mass7 :mass-leap mass-leap :mass-dur mass-dur)
    (punct-longest5-middle :long long :long-dur long-dur)
    (punct-longest-first :long-first long-first)
    (rem-before-rest)
    (rem-short :short short)
    (punct-first-short :fish fish :fish-dur fish-dur)
    (punct-short-between-long :shbelo shbelo)
    (reduce-long)
    (reduce-in-appoggiatura :reduce-in-app reduce-in-app)
    (rem-one-in-repetition)
    (rem-less-than-five :mean-weight-factor mean-weight-factor)
    )
  ;(distance)
)

;-------- punct. at rest -----------------------------

(defun punct-rest ()
 (let ((weight 10))
  (each-note-if
    (not (last?))
    (not (this 'rest))
    (next 'rest)
    (then
        (add-this-punct-list  (list 'rest weight))
        (add-this-weight weight)
        ))))


;--------massavskiljning genom tonhojdskommatering ---------------------


;behover aven behandling av sista och forsta tonen

(defun punct-mass (&key (mass1 10) (mass2 10) (mass3 6) (mass4 3) (mass5 4) 
                        (mass6 5)  (mass7 3) (mass-leap 1) (mass-dur 1))
 (let ((weight nil)(subcase))
  (each-note-if
    (not (first?))
    (not (last?))
    (not (first+1?))
    (not (last-1?))
    (this 'leap)
    (prev 'leap)
    (next 'leap)
    (not (last-1?))
    (> (abs (this 'leap)) 2)      ;leap
    (cond 
          ((and (up? (prev 'leap))   ;up/down/up 
                (down? (this 'leap)) 
                (up? (next 'leap))
                (> (abs (this 'leap)) (round (* 1.5 (abs (prev 'leap)))))
                (> (abs (this 'leap)) (round (* 1.5 (abs (next 'leap))))) )
           (setq subcase 'mass1)
           (setq weight mass1) ) 
          ((and (down? (prev 'leap))
                (up? (this 'leap)) 
                (down? (next 'leap))
                (> (abs (this 'leap)) (round (* 1.5 (abs (prev 'leap)))))
                (> (abs (this 'leap)) (round (* 1.5 (abs (next 'leap))))) )
           (setq subcase 'mass1)
           (setq weight mass1) ) 

          ((and (upstep? (prev 'leap))   ;upstep/downleap/downstep 
                (down? (this 'leap)) 
                (downstep? (next 'leap)) )
           (setq subcase 'mass2)
           (setq weight mass2) ) 
          ((and (downstep? (prev 'leap))
                (up? (this 'leap)) 
                (upstep? (next 'leap)) )
           (setq subcase 'mass2)
            (setq weight mass2) ) 

          ((and (uporsame? (prev 'leap)) ;uporsame/downleap/upsteporsame
                (down? (this 'leap)) 
                (upsteporsame? (next 'leap)) )
           (setq subcase 'mass3)
            (setq weight mass3) ) 
          ((and (downorsame? (prev 'leap))
                (up? (this 'leap)) 
                (downsteporsame? (next 'leap)) )
           (setq subcase 'mass3)
            (setq weight mass3) ) 

          ((and (upsteporsame? (prev 'leap)) ;upsteporsame/downleap/uporsame
                (down? (this 'leap)) 
                (uporsame? (next 'leap)) )
           (setq subcase 'mass4)
            (setq weight mass4) ) 
          ((and (downsteporsame? (prev 'leap))
                (up? (this 'leap)) 
                (downorsame? (next 'leap)) )
           (setq subcase 'mass4)
            (setq weight mass4) ) 

          ((and (up? (prev 'leap))       ;up/downleap/downstep 
                (down? (this 'leap)) 
                (downstep? (next 'leap)) )
           (setq subcase 'mass5)
           (setq weight mass5) ) 
          ((and (down? (prev 'leap))
                (up? (this 'leap)) 
                (upstep? (next 'leap)) )
           (setq subcase 'mass5)
            (setq weight mass5) ) 

          ((and (up? (prev 'leap))        ;up/up/downsteporsame
                (up? (this 'leap)) 
                (downsteporsame? (next 'leap)) )
           (setq subcase 'mass6)
            (setq weight mass6) ) 
          ((and (down? (prev 'leap))
                (down? (this 'leap)) 
                (upsteporsame? (next 'leap)) )
           (setq subcase 'mass6)
            (setq weight mass6) ) 

          ((and (upsteporsame? (prev 'leap))    ;upsteporsame/up/upsteporsame (upstep/up/upstep)
                (up? (this 'leap)) 
                (or (upsteporsame? (next 'leap))
                    (and (up? (next 'leap))
                         (> (abs (this 'leap)) (* 2 (abs (next 'leap)))) )))
           (setq subcase 'mass7)
            (setq weight mass7) ) 
          ((and (downsteporsame? (prev 'leap))
                (down? (this 'leap)) 
                (or (downsteporsame? (next 'leap))
                    (and (down? (next 'leap))
                         (> (abs (this 'leap)) (* 2 (abs (next 'leap)))) )))
           (setq subcase 'mass7)
            (setq weight mass7) ) 

             ) 
    (then
      (setq weight
       (* weight mass-leap
         (dr-linear
            (abs (this 'leap))
            4. 0.3 12. 1.7 )))
      (setq weight (round
       (* weight mass-dur
         (dr-linear
            (+ (prev2 'dr)(next 'dr)(this 'dr)
               (next 'dr)(next2 'dr) )
            1000. 1. 4000. 0. ))))
      (add-this-punct-list  (list subcase weight))
      (add-this-weight weight) )
   )))

;(defun foo (quant) (punct-mass)(punct-apply quant))


;------------- durational punctuation ----------------

;efter längsta bland 5
;bör hantera bundna som en lång
;for middle note
(defun punct-longest5-middle (&key (long 6.5) (long-dur 1))
 (let ((weight))
  (each-note-if
    (not (first?))
    (not (last?))
    (not (first+1?))
    (not (last-1?))
    (not (this 'rest))
    (not (prev 'rest))
    (not (next 'rest))
    (then
      (let ((ndr (this 'ndr)))
      (when (and
             (>= ndr (iget (- *i* 2) 'ndr))
             (>= ndr (iget (- *i* 1) 'ndr))
             (> ndr (iget (+ *i* 1) 'ndr))
             (> ndr (iget (+ *i* 2) 'ndr)) )
        (setq weight long) ;get proportion
        ; 0.5 -> 3 and 0.67 -> 6 and linear interpolation between
        (setq weight (* weight long-dur (dr-linear (/ ndr (punct-longest5-get-dur3)) 0.5 0.78 0.67 1.23)))
        (add-this-punct-list  (list 'long weight))
        (add-this-weight weight)
        ))))))


(defun punct-longest5-get-dur5 ()
  (let ((drtot 0.0))
    (loop for i from (- *i* 2) to (+ *i* 2) do
      (setq drtot (+ drtot (iget i 'ndr))) )
    drtot ))

(defun punct-longest5-get-dur3 ()
  (let ((drtot 0.0))
    (loop for i from (- *i* 1) to (+ *i* 1) do
      (setq drtot (+ drtot (iget i 'ndr))) )
    drtot ))


;first note or first note after rest        
(defun punct-longest-first (&key (long-first 5))
 (let ((weight))
  (each-note-if
    (or (first?)(prev 'rest))
    (not (this 'rest))
    (not (last?))
    (then
      (let ((ndr (this 'ndr)))
       (when 
         (> ndr (next 'ndr))
         (setq weight long-first)
         (add-this-punct-list  (list 'long weight))
         (add-this-weight weight)
        ))))))


;;-------- komma före kort mellan långa -------------

(defun punct-short-between-long (&key (shbelo 6))
 (let ((weight))
  (each-note-if
    (not (first?))
    (not (last?))
    (not (this 'rest))
    (not (prev 'rest))
    (not (next 'rest))
    (< (this 'ndr) (prev 'ndr))
    (< (this 'ndr) (next 'ndr))
    (then
       (setq weight shbelo)
        (add-prev-punct-list  (list 'shbelo weight))
        (add-prev-weight weight)
        ))))

;;-------- komma före lika korta före lång -------------


(defun punct-first-short (&key (fish 3.5) (fish-dur 1))
 (let ((weight))
  (each-note-if
    (not (first?))
    (not (last?))
    (not (this 'rest))
    (not (prev 'rest))
    (not (next 'rest))
    ;(not (prev 'long))
    (< (this 'ndr) (prev 'ndr))
    (= (this 'ndr) (next 'ndr))
    (then
       (setq weight fish)
       (setq weight (* weight fish-dur
          (dr-linear-limits (/ (prev 'dr)(this 'dr)) 1. 0. 4. 2.) ))
        (add-prev-punct-list  (list 'fish weight))
        (add-prev-weight weight)
        ))))


;;---------- reduce weight if several "long" rules ------------- 

(defun reduce-long ()
 (each-note-if
  (this :punct)
  (then
    (if (and
           (in-this-punct-list? 'fish)
           (in-this-punct-list? 'shbelo) )
        (if (> (this-punct-list-value 'fish)
               (this-punct-list-value 'shbelo) )
            (add-this-weight (- (this-punct-list-value 'shbelo)))  
            (add-this-weight (- (this-punct-list-value 'fish))) ))  
    (if (and
           (in-this-punct-list? 'long)
           (in-this-punct-list? 'shbelo) )
        (if (> (this-punct-list-value 'long)
               (this-punct-list-value 'shbelo) )
            (add-this-weight (- (this-punct-list-value 'shbelo)))  
            (add-this-weight (- (this-punct-list-value 'long))) ))  
    (if (and
           (in-this-punct-list? 'fish)
           (in-this-punct-list? 'long) )
        (if (> (this-punct-list-value 'fish)
               (this-punct-list-value 'long) )
            (add-this-weight (- (this-punct-list-value 'long)))  
            (add-this-weight (- (this-punct-list-value 'fish))) ))
            )))  



;;------- komma efter forhallning ---------------

;;0009/af changed error message
(defun punct-appoggiatura (&key (app 5))
  (if (not (get-first 'q) )
    (if (get-dm-var 'verbose-i/o) (Print-ll "Punctuation : no chord in first note - skipping punct-appoggiatura"))  
    (let ((q)(weight app))
      (each-note-if
        (not (last?))
        (then
          (if (this 'q) (setq q (this 'q)))
          (if (and (not (this 'rest))
                   (not (next 'rest))
                   (appoggiatura? q) )
            (then
              (add-next-punct-list  (list 'app weight))
              (add-next-weight weight)
              )))))))

;;(ifn (and (= (next 'f0) (iget (+ 2 *i*) 'f0))))

;;------- punct. before chord change if not upbeat ---------------

(defun punct-before-chord ()
  (if (not (get-first 'q))
    (if (get-dm-var 'verbose-i/o) (Print-ll "Punctuation : no chords - skipping punct-before-chord"))
    (let ((weight 4))
      (each-note-if
        (not (last?))
        (not (this 'rest))
        (next 'q)
        (not (upbeat-to-chord?))
        (then
          ;(print (this 'n))
          (add-this-punct-list  (list 'chord weight))
          (add-this-weight weight)
          )))))


;;--------------------------

(defun rem-in-stepmotion ()
  (each-note-if
     (not (first?))
     (this :punct)
     (prev 'leap)
     (this 'leap)
     (or (downstep? (prev 'leap))
         (upstep? (prev 'leap)) )
     (or (downstep? (this 'leap))
         (upstep? (this 'leap)) )
     (then
        (add-this-punct-list  (list 'step))
        (rem-this :weight)
        )))

;;--------------------------

(defun rem-short (&key (short 0.6))
  (let ((mean-dr (get-mean-dr)))
   (each-note-if
     (this :weight)
     (< (this 'dr) (* short mean-dr))
     (then 
       (add-this-punct-list 'short)
       (rem-this :weight) 
       ))))

(defun get-mean-dr ()
 ;(if (> (length *vlist*) 1)(error "get-mean-dr: works only monophonic"))
 (let ((drtot 0.0)
       (itot 0) )
   (each-note
     (setq drtot (+ drtot (this 'dr)))
     (incf itot) )
   (/ drtot itot) ))
   
;;--------------------------

(defun rem-before-rest ()
  (each-note-if
    (not (last?))
    (this :weight)
    (not (this 'rest))
    (not (next 'rest))
    (or (last-1?) (iget (+ *i* 2) 'rest))
    (then
       (add-this-punct-list 'last)
       (rem-this :weight) 
        )))

;-------------------

(defun rem-in-appoggiatura ()
  (each-note-if
    (not (first?))
    ;(this :weight)
    (prev :weight)
    (in-this-punct-list? 'app)
    (then
       (add-prev-punct-list 'apnxt)
       (rem-prev :weight)
       ))) 

(defun reduce-in-appoggiatura (&key (reduce-in-app 3))
  (each-note-if
    (not (first?))
    ;(this :weight)
    (prev :weight)
    (in-this-punct-list? 'app)
    (then
       (add-prev-punct-list 'apnxt)
       (if (< (prev :weight) (- reduce-in-app))
         (rem-prev :weight)
         (add-prev :weight reduce-in-app) )
       )))
      
;-----------------------------

(defun rem-one-in-repetition ()
  (each-note-if
    (not (last?))
    (this :weight)
    (next :weight)
    (= (this-f0) (next-f0))
    (then
      ;(print-ll "this w " (this :weight) "next w " (next :weight))
      (cond
       ((< (this :weight) (next :weight))
        (add-this-punct-list  'rep)
        (rem-this :weight) )
       ((> (this :weight) (next :weight))
        (add-next-punct-list  'rep)
        (rem-next :weight) )
       ((= (this :weight) (next :weight))
        (add-this-punct-list  'rep)
        (rem-this :weight) )
       ))))
    


;;--------------------------

(defun rem-less-than-five (&key (mean-weight-factor 0.6))
  (let ((mean-weight (get-mean-weight)))
   (each-note-if
     (this :weight)
     (< (this :weight) (* mean-weight-factor mean-weight)) ;0.6 original
     (then 
       (add-this-punct-list 'less)
       (rem-this :weight) 
       ))))

(defun get-mean-weight ()
 ;(if (> (length *vlist*) 1)(error 'get-mean-weight "works only monophonic" ""))
 (let ((weighttot 0.0)
       (itot 0) )
   (each-note-if
     (this :weight)
     (setq weighttot (+ weighttot (this :weight)))
     (incf itot) )
   (if (> itot 0) 
     (/ weighttot itot)
     0 )))
   
           

;;--------- apply the punctuation to dro ------------------


;;without weight considerations
(defun punct-apply (quant &key (dur 1) (duroff 1))
  (each-note-if
    (this :weight)
    (> (this :weight) 0)
    (not (last?))
    (not (next 'rest))
    (then
      (let ((addval (* quant (dr-linear-limits (this 'dr) 160. 20. 640. 60.))))
       (if (this 'dro)
           (add-this 'dro (* duroff addval))
           (set-this 'dro (* duroff addval)) )
       (add-this 'dr (* dur addval 0.45)) ))))

;Drake&Palmer (1993) kind of marking
(defun punct-apply-before (quant)
  (each-note-if
    (this :weight)
    (> (this :weight) 0)
    (not (last?))
    (not (next 'rest))
    (then
      (let ((addvalprev (* quant (dr-linear-limits (prev 'dr) 160. 20. 640. 60.)))
            (addvalthis (* quant (dr-linear-limits (this 'dr) 160. 20. 640. 60.))))
       (if (this 'dro)
           (add-this 'dro addvalthis)
           (set-this 'dro addvalthis) )
      ; (if (prev 'dro)
      ;     (add-prev 'dro (/ addvalprev 2))
      ;     (set-prev 'dro (/ addvalprev 2)) )
       (add-prev 'dr (* addvalprev 0.45))
       (add-this 'dr (* addvalthis 0.45))
       ))))


;a first attempt at "klasfras" marking of the boundaries:
;a amplitude dip in the last note of the group
(defun punct-apply-dip (quant)
  (each-note-if
    (this :weight)
    (> (this :weight) 0)
    (not (last?))
    (not (next 'rest))
    (then
      (let ((addval (* quant (dr-linear-limits (this 'dr) 160. 20. 640. 60.))))
       (set-this 'vol (interpolate-list 
                       (list 0 0 (* (this 'dr) 0.50) 0 (* (this 'dr) 0.99) (- 0 (* addval 0.1)) (this 'dr) 0)))
       (add-this 'dr (* addval 0.45)) ))))


;---- more access/help functions ---------------------------------

(defun up? (leap) (if (> leap 0) leap nil))  
(defun uporsame? (leap) (if (>= leap 0) leap nil))  
(defun down? (leap) (if (minusp leap) leap nil))  
(defun downorsame? (leap) (if (or (minusp leap)(zerop leap)) leap nil))  
(defun upstep? (leap)(if (and (> leap 0)(< leap 3)) leap nil))
(defun upsteporsame? (leap)(if (and (>= leap 0)(< leap 3)) leap nil))
(defun downstep? (leap)(if (and (minusp leap)(> leap -3)) leap nil))
(defun downsteporsame? (leap)
    (if (or (zerop leap)(and (minusp leap)(> leap -3))) leap nil))

(defun in-this-punct-list? (name)
  (let ((punctl (this :punct))
        (pair)
        (result nil) )
     (while punctl
       (setq pair (pop punctl))
       (if (and (listp pair) (equal (car pair) name))
           (setq result t) ))
     result ))

(defun in-prev-punct-list? (name)
  (let ((punctl (prev :punct))
        (pair)
        (result nil) )
     (while punctl
       (setq pair (pop punctl))
       (if (and (listp pair) (equal (car pair) name))
           (setq result t) ))
     result ))

(defun this-punct-list-value (name)
  (let ((punctl (this :punct))
        (value nil)
        (pair '()) )
     (while punctl
       (setq pair (pop punctl))
       (if (and (listp pair) (equal (car pair) name))
           (setq value (cadr pair)) ))
     value ))

(defun add-this-punct-list (thing)
  (cond ((not (this :punct))
         (set-this :punct (list thing)) )
        ((listp (this :punct))
         (set-this :punct (append (this :punct) (list thing))) )
        (t (error 'add-this-punct-list "prop punct not a list" (this :punct)))
        ))

(defun add-next-punct-list (thing)
  (cond ((not (next :punct))
         (set-next :punct (list thing)) )
        ((listp (next :punct))
         (set-next :punct (append (next :punct) (list thing))) )
        (t (error 'add-next-punct-list "prop punct not a list" (next :punct)))
        ))

(defun add-prev-punct-list (thing)
  (cond ((not (prev :punct))
         (set-prev :punct (list thing)) )
        ((listp (prev :punct))
         (set-prev :punct (append (prev :punct) (list thing))) )
        (t (error 'add-prev-punct-list "prop punct not a list" (prev :punct)))
        ))

(defun add-this-weight (val)
  (if (this :weight)
      (add-this :weight val)
      (set-this :weight val) ))

(defun add-next-weight (val)
  (if (next :weight)
      (add-next :weight val)
      (set-next :weight val) ))

(defun add-prev-weight (val)
  (if (prev :weight)
      (add-prev :weight val)
      (set-prev :weight val) ))

;(defun foo (thing)(each-note (add-to-punct-list thing)))  

#|
(defun appoggiatura? (q)
  (if (and 
       (not (next 'q))
       (not (this 'rest))
       (not (next 'rest))
       (not (last?)) )
    (then
      ;(print (this 'n)(next 'n))
      (let* ((qtonnr (tone-to-tonnr (car q)))
             (rel-tonnr (mod (- (+ (note-to-tonnr (this 'n)) 12) qtonnr) 12))
             (next-rel-tonnr (mod (- (+ (note-to-tonnr (next 'n)) 12) qtonnr) 12))
             )
        ;(print-ll "this tonnr " rel-tonnr "next tonnr " next-rel-tonnr)
        (if
          (or
           (and (plusp (this 'leap))
                (member (list rel-tonnr next-rel-tonnr)
                        '((11 0) (6 7) (3 4) (2 4) (2 3))
                         :test #'equal))
           (and (minusp (this 'leap))
                (member (list rel-tonnr next-rel-tonnr)
                        '((1 0) (2 0) (7 0) (9 7) (8 7) (5 3) (5 4))
                         :test #'equal))
           (and (plusp (this 'leap))
                (major? q)
                (member (list rel-tonnr next-rel-tonnr)
                        '((9 10))
                         :test #'equal))
           )
          t
          nil )))))
|#
;;0009/af new string chord format
(defun appoggiatura? (q)
  (if (and 
       (not (next 'q))
       (not (this 'rest))
       (not (next 'rest))
       (not (last?)) )
    (then
      ;(print (this 'n)(next 'n))
      (let* ((qtonnr (tone-to-tonnr (chord-to-root q)))
             (rel-tonnr (mod (- (+ (note-to-tonnr (this 'n)) 12) qtonnr) 12))
             (next-rel-tonnr (mod (- (+ (note-to-tonnr (next 'n)) 12) qtonnr) 12))
             )
        ;(print-ll "this tonnr " rel-tonnr "next tonnr " next-rel-tonnr)
        (if
          (or
           (and (plusp (this 'leap))
                (member (list rel-tonnr next-rel-tonnr)
                        '((11 0) (6 7) (3 4) (2 4) (2 3))
                         :test #'equal))
           (and (minusp (this 'leap))
                (member (list rel-tonnr next-rel-tonnr)
                        '((1 0) (2 0) (7 0) (9 7) (8 7) (5 3) (5 4))
                         :test #'equal))
           (and (plusp (this 'leap))
                (major? q)
                (member (list rel-tonnr next-rel-tonnr)
                        '((9 10))
                         :test #'equal))
           )
          t
          nil )))))
;;0009/af fixed for chord string format
(defun upbeat-to-chord? ()
 (if (and 
      (next 'q)
      (not (this 'rest))
      (not (next 'rest))
      (not (last?)) )
  (then
    ;(print (this 'n)(next 'n))
    (let* ((q (next 'q))
           (qtonnr (tone-to-tonnr (chord-to-root q)))
           (rel-tonnr (mod (- (+ (note-to-tonnr (this 'n)) 12) qtonnr) 12))
           (next-rel-tonnr (mod (- (+ (note-to-tonnr (next 'n)) 12) qtonnr) 12))
          )
     ;(print "this tonnr " rel-tonnr "next tonnr " next-rel-tonnr)
      (if
       (or
          (and (plusp (this 'leap))
               (member (list rel-tonnr next-rel-tonnr)
                 (list '(11 0) '(6 7) '(3 4) '(2 4) '(2 3)) ))
          (and (minusp (this 'leap))
               (member (list rel-tonnr next-rel-tonnr)
                 (list '(1 0) '(2 0) '(7 0) '(9 7) '(8 7) '(5 3) '(5 4)) ))
          (and (plusp (this 'leap))
               (major? q)
               (member (list rel-tonnr next-rel-tonnr)
                 (list '(9 10)) ))
                 )
       t
       nil )))))

;(defun foo ()(each-note-if (upbeat-to-chord?)(print (this 'n))))

(defun tonnr-to-tonnr-above-root (tonnr qnr)
   (mod (- tonnr qnr) 12) )

(defun note-to-tonnr-above-root (note qnr)
   (mod (- (tone-to-tonnr (note-to-tone note)) qnr) 12) )

;;0009/af also chord string format
(defun major? (q)
  (if (stringp q) (setq q (qname-to-qlist q)))
  (eq 4 (- (tone-to-tonnr (cadr q))(tone-to-tonnr (car q)))) )

;(defun foo ()(major? '("C" "Eb" "G")))


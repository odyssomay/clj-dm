;;;-*-Mode: LISP; Package: DM -*-

;;BAR: Bresin Articulation Rules

;;2000/Roberto Bresin
;; 12/2000: Roberto Bresin added pause-after-long: articulation rule for organ music
;; 03/2001: Roberto Bresin, new version of Score-staccato-art
;; 04/2001: af, added score-legato-last-note-art
;; 08/2001: af, removed 'set-this staccato t from the function score-legato-art1
;; 090422/af moved overall-articulation here
;; 130722/af added pedalling


(in-package "DM")

;;(defvar *legato*)
(defparameter *legato* 0)

;;(defvar *staccato*)
(defparameter *staccato* 0)


;;-------- Score legato articulation --------------------  

(defun score-legato-art (quant)
  (score-legato-art1 quant)
  (if (not (get-dm-var 'rule-debug-info)) (rem-all 'scorelegatoddr))
 )
 

(defun score-legato-art1 (quant)
  (each-note-if
   (or (and (not (this 'rest)) (this 'legato-start)) (and (not (this 'rest)) (= *legato* 1)))
   (then
    (setf *legato* 1)
    (if (next 'legato-end) ;; the last note in a series of legato marked notes is NOT played legato
        (setf *legato* 0))
    (cond ((and
            (> quant 1)
            (<= quant 5) )
           (then 
            (set-this 'scorelegatoddr
                      ;; Original equation:   (dr * (0.00005 * quant - 0.011)+ 1.105*quant+16.063) * dr / 100
                      (/ (* (+ (+ (* (- (* 0.00005 quant) 0.011) (this 'dr)) (* 1.105 quant)) 16.063) (this 'dr)) 100))) )
          
          ((and
            (> quant 0)
            (<= quant 1) )
           (then 
            (set-this 'scorelegatoddr                    
                      ;; Original equation:   (dr * (- 0.0043*quant - 0.0066)+ (5.8533* quant + 11.315))* dr / 100
                      (/ (* (+ (+ (* ( - (* -0.0043 quant) 0.0066 ) (this 'dr)) (*  5.8533 quant)) 11.315) (this 'dr)) 100))) )
          )
    
    (if (and (not (next 'rest)) (not (zerop (abs (- (next-f0) (this-f0))))))
        (if (this 'dro)
            (add-this 'dro (* (this 'scorelegatoddr) -1))
          (set-this 'dro (*(this 'scorelegatoddr) -1))
          ))
    ) ))

(defun check-for-legato-marks ()
 (let ((found nil))
  (block loop
  (each-note-if
    (this 'legato-start)
    (then
      (setq found t)
      (return-from loop) )))
  found))


;detach last note/af
;;quant=1 gives a 60 ms pause
(defun score-legato-last-note-art (quant)
  (each-note-if
   (this 'legato-end)
   (not (this 'rest))
   (not (last?))
   (not (next 'rest))
   (then
      (if (this 'dro)
       (add-this 'dro (* quant 60))
     (set-this 'dro (* quant 60))
     ))))



#|
;;-------- Score staccato articulation --------------------  1st VERSION, 2000

(defun score-staccato-art (quant)
  (score-staccato-art1 quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'scorestaccatoddr))
  )
 
(defun score-staccato-art1 (quant)
 (each-note-if
  (and (not (this 'rest)) (this 'staccato))
  (then
    (cond ((and
            (> quant 1)
            (<= quant 5) )
           (then 
            (set-this 'scorestaccatoddr
              (* (* (+ (* 0.0325 quant) 0.9675)  (this 'dr)) 0.665))) ) 
          ((and
            (> quant 0)
            (<= quant 1) )
           (then 
            (set-this 'scorestaccatoddr
               (* (* (+ (* 0.6889 quant) 0.3111)  (this 'dr)) 0.665))) )                      
     )
   (if (this 'dro)
       (add-this 'dro (this 'scorestaccatoddr))
     (set-this 'dro (this 'scorestaccatoddr))
     )) ))

|#


#|
;;-------- Score staccato articulation --------------------  2nd VERSION, 2001 (according "Virtual Virtuosity", page 11)

(defun score-staccato-art (quant &key (tempo-indication 1))
  (score-staccato-art1 quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'scorestaccatoddr))
  )
 
(defun score-staccato-art1 (quant)
 (each-note-if
  (and (not (this 'rest)) (this 'staccato))
  (then
    (cond ((and
            (> quant 1)
            (<= quant 5) )
           (then 
            (set-this 'scorestaccatoddr
              (* (* (+ (* 0.0325 quant) 0.9675)  (this 'dr)) 0.665))) ) 
          ((and
            (> quant 0)
            (<= quant 1) )
           (then 
            (set-this 'scorestaccatoddr
                      (* (* (+ (* 0.6889 quant) 0.3111)  (this 'dr)) 0.665))) ) 
          (set-this 'scorestaccatoddr (* (this 'scorestaccatoddr) tempo-indication))
     )
   (if (this 'dro)
       (add-this 'dro (this 'scorestaccatoddr))
     (set-this 'dro (this 'scorestaccatoddr))
     )) ))

|#


#|
;;-------- Score staccato articulation --------------------  3rd VERSION, 2001.03.27 (according "Virtual Virtuosity", page 11)

(defun score-staccato-art (quant &key (tempo-indication 1))
  (score-staccato-art1 quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'scorestaccatoddr))
  )
 
(defun score-staccato-art1 (quant)
 (each-note-if
  (or (and (not (this 'rest)) (this 'staccato-start)) (and (not (this 'rest)) (= *staccato* 1)) 
      (and (not (this 'rest)) (this 'staccato)))
;;  (and (not (this 'rest)) (this 'staccato))
   (then
    (setf *staccato* 1)
    (if (or (this 'staccato-end) (this 'staccato)) ;; the last note in a series of staccato marked notes is also played staccato
            (setf *staccato* 0))
    (cond ((and
            (> quant 1)
            (<= quant 5) )
           (then 
            (set-this 'scorestaccatoddr
              (* (* (+ (* 0.0325 quant) 0.9675)  (this 'dr)) 0.665))) ) 
          ((and
            (> quant 0)
            (<= quant 1) )
           (then 
            (set-this 'scorestaccatoddr
                      (* (* (+ (* 0.6889 quant) 0.3111)  (this 'dr)) 0.665))) ) 
          (set-this 'scorestaccatoddr (* (this 'scorestaccatoddr) tempo-indication))
     )
   (if (this 'dro)
       (add-this 'dro (this 'scorestaccatoddr))
     (set-this 'dro (this 'scorestaccatoddr))
     )) ))
|#

;;-------- Score staccato articulation --------------------  LATEST VERSION, 2001.03.29 (according "Virtual Virtuosity", page 11)

(defun score-staccato-art (quant &key (tempo-indication 1))
  (score-staccato-art1 quant)
 (if (not (get-dm-var 'rule-debug-info)) (rem-all 'scorestaccatoddr))
  )
 
(defun score-staccato-art1 (quant)
 (each-note-if
  (or (and (not (this 'rest)) (this 'staccato-start)) (and (not (this 'rest)) (= *staccato* 1)) 
      (and (not (this 'rest)) (this 'staccato)))  
  (then
    ;(set-this 'staccato t) ;;funkar ej for 'staccato-start/af
    (setf *staccato* 1)
    (ckeck-next-staccato-tone)
    (if (or (this 'staccato-end) (this 'staccato)) ;; the last note in a series of staccato marked notes is also played staccato
            (setf *staccato* 0))
    (cond ((and
            (> quant 1)
            (<= quant 5) )
           (then 
            (set-this 'scorestaccatoddr
              (* (* (+ (* 0.0325 quant) 0.9675)  (this 'dr)) 0.665))) ) 
          ((and
            (> quant 0)
            (<= quant 1) )
           (then 
            (set-this 'scorestaccatoddr
                      (* (* (+ (* 0.6889 quant) 0.3111)  (this 'dr)) 0.665))) ) 
          (set-this 'scorestaccatoddr (* (this 'scorestaccatoddr) tempo-indication))
     )
   (if (this 'dro)
       (add-this 'dro (this 'scorestaccatoddr))
     (set-this 'dro (this 'scorestaccatoddr))
     )) ))


(defun ckeck-next-staccato-tone ()
    
    )
;;-------- Note repetition --------------------  

#|
;the repetition of the same note
(defun repetition-articulation-dro (quant)
  (let ((df0))
(each-note
  (ifn (or (last?)
           (next 'rest)
           (this 'rest)
           (this 'bind)
           (this 'dro) )  ;not if f ex phrases
    (progn
     (setq df0 (abs (- (next-f0) (this-f0))))
     (cond ((zerop df0)
            (set-this 'dro (round (*  quant 35.)))
            )))))))

|#
;end

;;-------- Pause after long --------------------  

;; pause-after-long: set a pause after notes with duration longer than "limit"
(defun pause-after-long (quant &key (limit 399))
  
  (pause-after-long1 limit)
 
  (duration-contrast-rem-double-duration)
  (pause-after-long-art quant)
 
  (if (not (get-dm-var 'rule-debug-info)) (rem-all 'durcontddr)) 
  
  )

(defun pause-after-long1 (limit)
 (each-note-if
  (not (this 'rest)) 
  (then
   (let ((dr (this 'dr)))
    (cond ((> (this 'dr) limit)
        (then (set-this 'durcontddr (infix (0.25 * dr))) ))) ))
           )
)

(defun pause-after-long-art (quant)
  (let ((out-of-legato t) (df0 1)) ;; df0=1 is an initialisation value
    (each-note
     (if (this 'legato-end)
         (setf out-of-legato t))
     (if (this 'legato-start)
         (setf out-of-legato nil))
     (if (this 'staccato)
         (setf out-of-legato nil))
     
     
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
           (add-this 'dro ( + (* quant (this 'durcontddr))))
         (set-this 'dro ( + (* quant (this 'durcontddr))))
         
         )
       ;;       (add-this 'dr ( + (* quant (this 'durcontddr)))
       )


          ))))


;;------------ overall articulation -------------------------------

#|
(defun overall-articulation (quant)
  (each-note-if
   (not (last?))
   (not (this 'rest))
   (not (next 'rest))
   (then
    (if (this 'dro)
     (add-this 'dro (* quant (this 'dr) 0.25))
      (set-this 'dro (* quant (this 'dr) 0.25))
      ))))

;031215/af also on note before rest
(defun overall-articulation (quant)
  (each-note-if
   (not (last?))
   (not (this 'rest))
   ;(not (next 'rest))
   (then
    (if (this 'dro)
     (add-this 'dro (* quant (this 'dr) 0.25))
      (set-this 'dro (* quant (this 'dr) 0.25))
      )
    (if (> 100 (- (this 'dr) (or (this 'dro) 0)))
        (print-ll "overall-articulation, duration shorter than 100 ms: " (- (this 'dr) (or (this 'dro) 0))) )
    )))
|#

;031215/af also on note before rest
;031215/af IOI not shorter than 100 ms
;081023/af not on tied note
(defun overall-articulation (quant)
  (each-note-if
   (not (last?))
   (not (this 'rest))
   (not (or (this 'tie) (this 'bind)))
   (> (this 'dr) 100)
   ;(not (next 'rest))
   (then
    (if (this 'dro)
     (add-this 'dro (* quant (this 'dr) 0.25))
      (set-this 'dro (* quant (this 'dr) 0.25))
      )
;;;    (if (> 100 (- (this 'dr) (or (this 'dro) 0)))
;;;        (print-ll "overall-articulation, duration shorter than 100 ms: " (- (this 'dr) (or (this 'dro) 0))) )
    )))

;applies a lower boundary for the articulation
;quant = 1 gives 20 ms min duration
(defun articulation-social-care (quant)
  (let ((dr-min (* 20 quant)))
  (each-note-if
   (this 'dro)
   (then
    (if (< (- (this 'dr) (this 'dro)) dr-min)
        (set-this 'dro (max 0 (- (this 'dr) dr-min))) ;set the boundary, if negative set zero
      )))))


;;--------------- simple pedalling-----------------------
;;130722/af

(defun piano-pedalling-simple (quant)
  (let ((down? nil))
    (each-note-if
     (this 'pedal)
     (then
      (when (and (= (this 'pedal) 1) (not down?))
        (set-this 'pedal-down (* quant 30))
        (setq down? t) )
      (when (and (= (this 'pedal) 0) down?)
        (set-this 'pedal-up 0)
        (setq down? nil) )
      (when (and (= (this 'pedal) 1) down?)
        (set-this 'pedal-up 0)
        (set-this 'pedal-down (* quant 100))
        (setq down? t) )
      ))))
    
    
    
    
    
    
    
    
    
    
    
    
    
      
      


;;some macros and rules for parallel processing of the voices
;;1986/Anders Friberg. 
;;9201 cl /af
;;971118/af converted to DM2

(in-package :dm)


(defvar *cur-notes*)
(defvar *all-notes*)
(defvar *ndr-to-next*)

;;---macros------------------------------


;steps in parallel through the voices
;one step for each new note in any voice
;global parameters that can be used within the macro:
; *all-notes* = all the current notes in a vertical cut (alist)
; *cur-notes* = all notes that is new for each step (alist)
; *ndr-to-next* = duration from the current step (time-slice) to next note
(defmacro p-each-note (&body body)
  `(let ((nv) (nv-1) (nvec) (tonvec)
         (vvec) (drmin) (vnrlst ())(ndrvec)(vnr))
     (declare (special nv-1 vvec ndrvec))
     (setq nv (length (active-track-list *active-score*)))
     (setq nv-1 (1- nv))
     (setq nvec (make-array (list nv) :initial-element 0))   ;index vector
     (setq tonvec  (make-array (list nv) :initial-element 0))  ;current element object array
     ;ndr to end of note from current vertical view-point:
     (setq ndrvec  (make-array (list nv) :initial-element 0))
     (setq vvec (apply 'vector (active-track-list *active-score*)))     ;track object vector (voice vector)
     
     (loop for i from 0 to nv-1 do                    ;get the first notes 
          (setf (aref tonvec i) (nth 0 (segment-list (aref vvec i))))       
          (setf (aref ndrvec i) (get-var (aref tonvec i) 'ndr)) )

     (setq *all-notes* nil)          ;build *all-notes*
     (loop for i from 0 to nv-1 do
          (newr *all-notes* (cons (aref vvec i) (aref nvec i)) ))
     (setq *cur-notes* *all-notes*)
     
     (untilexit end
                
        (setq drmin 32000)    ;compute the *ndr-to-next* value 
        (loop for i from 0 to nv-1 do             ;find the smallest duration
             (cond ((> drmin (aref ndrvec i))
                    (setq drmin (aref ndrvec i))
                    (setq vnr i) )))
        (setq *ndr-to-next* drmin)
                
        ,@body
                
        (setq vnr 0)
        (setq drmin 32000)
        (loop for i from 0 to nv-1 do             ;find the smallest duration
             (cond ((> drmin (aref ndrvec i))
                    (setq drmin (aref ndrvec i))
                    (setq vnr i) )))
        (setq vnrlst ())              ;the list of voice indexes w/ smallest dr
        (loop for i from 0 to nv-1 do             ;flera toner samtidigt?
             (cond ((= (round drmin) (round (aref ndrvec i)))
                    (setq vnrlst (append1 vnrlst i)) )))
        (loop for i from 0 to nv-1 do             ;minska alla ndr med drmin
             (setf (aref ndrvec i) (- (aref ndrvec i) drmin)) )
        
        (dolist (vnr vnrlst)           ;for all new notes
                 (setf (aref nvec vnr) (1+ (aref nvec vnr))) ;incr. note-counter
                 (ifn (= (length (segment-list (aref vvec vnr))) ;if not 1+last
                         (aref nvec vnr) )  
                      (setf (aref tonvec vnr) (nth (aref nvec vnr) (segment-list (aref vvec vnr))))
                      (return-from end) )      
                 (setf (aref ndrvec vnr) (get-var (aref tonvec vnr) 'ndr)) )
        
        (setq *cur-notes* nil)          ;build *cur-notes*
        (dolist (vnr vnrlst)           ;for all new notes
                 (newr *cur-notes* 
                       (cons (aref vvec vnr) (aref nvec vnr)) ))
        (setq *all-notes* nil)          ;build *all-notes*
        (loop for i from 0 to nv-1 do
             (newr *all-notes* (cons (aref vvec i) (aref nvec i)) ))
        )))


;step one bar in the voices in parallel
;*cur-notes* holds a  list of the current notes
;each element is a cons'ed pair consisting of the voice name
;and the index within that voice
#|
(defmacro p-each-bar (&body body)
  `(let ((barnr 1) (*cur-notes*))
     (untilexit no-more-bars
                (mapc 'p-each-bar-aux
                      *vlist* )
                ,@body
                (incf barnr) 
                (setq *cur-notes* nil)
                )))
(defun p-each-bar-aux (v)
  (let ((tonnr (i?-barnr v barnr)))
   (ifn tonnr (return-from no-more-bars))
   (setq *cur-notes* (acons v tonnr *cur-notes*)) ))
|#
(defmacro p-each-bar (&body body)
  `(let ((barnr 1) (*cur-notes*))
     (untilexit no-more-bars
        (dolist (v (active-track-list *active-score*))
          (let ((tonnr (i?-barnr v barnr)))
            (ifn tonnr (return-from no-more-bars))
            (setq *cur-notes* (acons v tonnr *cur-notes*)) ))
        ,@body
        (incf barnr) 
        (setq *cur-notes* nil)
        )))


;;-- access functions ---------------

;returns an alist of the voice and property value 
;for each voice and note in *cur-notes*
(defun p-this (prop)
  (let ((l nil))
    (dolist (pair *cur-notes*)
      (push (v-iget pair prop) l))
    (remove nil l)))

(defun p-this-f0 ()
  (let ((l nil))
    (dolist (pair *cur-notes*)
      (push (v-iget-f0 pair) l))
    (remove nil l)))

(defun p-set-this (prop val)
  (dolist (pair *cur-notes*)
    (v-iset pair prop val)
    ))

(defun p-add-this (prop val)
  (dolist (pair *cur-notes*)
    (v-iset pair prop (+ val (v-iget pair prop)))
    ))

;returns an alist of the voice and property value 
;for each voice and note in *all-notes* 
(defun p-this-all (prop)
  (let ((l nil))
    (dolist (pair *all-notes*)
      (push (v-iget pair prop) l))
    (remove nil l)))

(defun p-set-this-all (prop val)
  (dolist (pair *all-notes*)
    (v-iset pair prop val)
    ))

;get the prop value for the specified note
(defun v-iget (vi prop)
  (let* ((v (car vi))(i (cdr vi))
         (cur-prop (get-var (nth i (segment-list v)) prop)))
    (if cur-prop (cons v cur-prop) nil) ))

;get the top note
(defun v-iget-f0 (vi)
  (let* ((v (car vi))(i (cdr vi))
         (f0 (get-var (nth i (segment-list v)) 'f0)))
    (if f0 (cons v (if (listp f0) (car f0) f0)) nil) ))


;set the prop value for the specified note
(defun v-iset (vi prop value)
  (let ((v (car vi))(i (cdr vi)))
    (set-var (nth i (segment-list v)) prop value) ))

;get the prop value in the current note in the specified voice
(defun v-this (v prop)
  (get-var (nth (cdr (assoc v *cur-notes*))
            (segment-list v) )
       prop) )

;get the prop value from curren or last current in the specified voice
(defun v-this-all (v prop)
  (get-var (nth (cdr (assoc v *all-notes*))
            (segment-list v) )
       prop) )

;get the  prop list in the current note in the specified voice
(defun v-this-plist (v)
  (var-list (nth (cdr (assoc v *cur-notes*))
                     (segment-list v) )))

;set the prop value in the current note in the specified voice
(defun v-set-this (v prop value)
  (set-var (nth (cdr (assoc v *cur-notes*)) (segment-list v)) prop value) )

(defun v-add-this (v prop value)
  (v-set-this v prop (+ (v-this v prop) value)) )

(defun v-set-next (v prop value)
  (set-var (nth (1+ (cdr (assoc v *cur-notes*))) (segment-list v)) prop value) )

(defun v-set-prev (v prop value)
  (set-var (nth (1- (cdr (assoc v *cur-notes*))) (segment-list v)) prop value) )

;;--- utility functions -----------

;borde checka alla!!!!!!!!!!!
(defun p-first? ()
  (zerop (cdar *cur-notes*)) )

;(defun p-last? ()
;  (= (cdar *cur-notes*) (v-i?last (caar *cur-notes*))) )

(defun p-last? ()
  (let ((last? t))
    (dolist (vi *cur-notes*)
      (if (not (= (cdr vi) (v-i?last (car vi))))
        (setq last? nil) )
      )
    last?))

;returns the pair the number of the note with property prop after note i
;if not found ->nil
(defun v-i?next (vi prop)
  (let* ((v (car vi))(i (cdr vi))
         (*v* (segment-list v)))
    (untilexit end
               (cond ((iget (incf i) prop)
                      (return-from end (cons v i)))
                     ((> i (length *v*))
                      (return-from end nil)) ))))

;returns the pair the number of the note with property prop after note i
;if not found -> last note
(defun v-i?next-or-last (vi prop)
  (let* ((v (car vi))(i (cdr vi))
         (*v* (segment-list v)))
    (untilexit end
               (cond ((iget (incf i) prop)
                      (return-from end (cons v i)))
                     ((> i (length *v*))
                      (return-from end (1- (length *v*)))) ))))

;returns the number of the last note
(defun v-i?last (v)
  (1- (length (segment-list v))) )

;return note number from voice and bar number
;else nil
(defun i?-barnr (v barnr)
  (let ((*v* v)
        (*rule-macro-context* 'each-track) )
    (block found
      (each-note-if
        (this 'bar)
        (= (this 'bar) barnr)
        (then
          (return-from found *i*) ))
      nil )))

;if same: take only one
(defun p-min (pair-alist)
  (declare (special pair-alist))
  (car (sort pair-alist #'(lambda (x y) (< (cdr x) (cdr y))))) )

;if same rounded return a list of those
(defun p-min-all (pair-alist)
  (declare (special pair-alist))
  (if (> (length pair-alist) 1)
    (let ((all (sort pair-alist #'(lambda (x y) (< (cdr x) (cdr y)))))
          (minl) )
      (let ((pair (pop all)))
        (newr minl pair)
        (while (and (not (null all))
                    (= (round (cdr pair)) (round (cdar all))) )
          (setq pair (pop all))
          (newr minl pair) )
        minl ))
    pair-alist ))


(defun p-max (pair-alist)
  (declare (special pair-alist))
  (car (sort pair-alist #'(lambda (x y) (> (cdr x) (cdr y))))) )

(defun v-to-ni (v)
  (cdr (assoc v *cur-notes*)) )




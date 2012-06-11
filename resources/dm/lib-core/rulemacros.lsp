;;;-*-Mode: LISP; Package: DM -*-
;;
;; ************************************************************
;;   Defines the basic rule functions and macros serial rules
;; ************************************************************
;;
;; 86 /Anders Friberg
;; 9111 Transfered to CL2
;; 9702/AF Completely new version for the new data structure
;; 0008/af fixed all set-dr access functions

(in-package :dm)

(defvar *rule-macro-context* nil)

;; ----------rule macros----------------------------------
;;
;; iterative macros that goes thru all tracks in *active-score* 
;; 
;; *active-score* must contain the score to be processed
;; *v* contains the current list of  objects
;; *i* is the current index to the current object in *v*
;; format: (each-note <s1>....<sN>)

;;; 
;;; ------ permisible nesting combinations by the user -------------
;;; 
;;; The complete nesting hierarchy of macros:
;;; 
;;; (each-track
;;;  (each-group
;;;   (each-note
;;;    (each-segment
;;; only each-track in combination with one other is allowed right now
;;; 
;;; Any of the above macros can be missing. 
;;; If one of the levels is missing the lower level macro iterate over all
;;; groups or segments.
;;; 
;;; (each-note
;;; 
;;; 
;;; The access functions operate on the whole group or a single segment
;;; depending on the macro context.
;;; 
;;; (each-track
;;;   (this-dr)                     ;get the duration of each track
;;;   (each-note
;;;     (this-dr)                  ;get the duration of each note
;;; ------------------------------------------------------
;;; Each macro will assign the current object to the variables:
;;; *this-track*, *this-group*, *this-note*, *this-segment*, respectively
;;;

;;these vars are only assigned within a rulemacro- no global value is used
;(defvar *this-score*)
(defvar *this-track*)
(defvar *this-group*)
(defvar *this-note*)
(defvar *this-segment*)
(defvar *i*)  ;index to current object
(defvar *v*)
(defvar *v-length*)


;a general function that loops over all objects in object-list
;and evaluates body each time
;the objects can be either tracks, meta-tracks or segments
;;; (defmacro each-object (object-list &body body)
;;;   `(let ((*v* ,object-list))
;;;      (declare (special *v*))
;;;       (dotimes (*i* (length *v*))
;;;        (declare (special *i*))
;;;        ,@body) ))
;;; 
;;; (defmacro each-object (object-list object-var &body body)
;;;    `(let ((*v* ,object-list))
;;;        (declare (special *v*))
;;;        (loop for i from 0 while (< i (length *v*)) do
;;;          (let ((*i* i))
;;;             (declare (special *i*))
;;;             (set ,object-var (nth *i* *v*))
;;;             ,@body) )))

;including environment vars such as *this-track* 
; and support for remove-this-segment  
(defmacro each-object (object-list object-var &body body)
   `(block each-object-loop
         (let ((*i* 0)(*v* ,object-list)(*v-length* (length ,object-list)))
            (declare (special *i* *v* *v-length*))
            (loop
              (if (>= *i* *v-length*) (return-from each-object-loop))
              (set ,object-var (nth *i* *v*))
              ,@body
              (incf *i*)) )))

;;; (defmacro each-object-if (object-list &body body)
;;;    (let ((ifpart (reverse (cdr (reverse body))))
;;;          (thenpart (last body)) )
;;;       `(let ((*v* ,object-list))
;;;           (declare (special *v*))
;;;           (dotimes (*i* (length *v*))
;;;              (declare (special *i*))
;;;              (if (and ,@ifpart)
;;;                 ,@thenpart)))))


(defmacro each-object-if (object-list object-var &body body)
   (let ((ifpart (reverse (cdr (reverse body))))
         (thenpart (last body)) )
   `(block each-object-loop
         (let ((*i* 0)(*v* ,object-list)(*v-length* (length ,object-list)))
            (declare (special *i* *v* *v-length*))
            (loop
              (if (>= *i* *v-length*) (return-from each-object-loop))
              (set ,object-var (nth *i* *v*))
             (if (and ,@ifpart)
                ,@thenpart)
              (incf *i*) )))))
;------ track-----------------------

(defmacro each-track (&body body)
   `(let ((*rule-macro-context* 'each-track))
       (declare (special *rule-macro-context*))
       (each-object (active-track-list *active-score*) '*this-track* ,@body) ))

;a special for going thru also inactivated tracks
;can be wrapped around 'each-note
(defmacro each-track-all-tracks (&body body)
   `(let ((*rule-macro-context* 'each-track))
       (declare (special *rule-macro-context*))
       (each-object (track-list *active-score*) '*this-track* ,@body) ))


(defmacro each-track-if (&body body)
   `(let ((*rule-macro-context* 'each-track))
       (declare (special *rule-macro-context*))
       (each-object-if (active-track-list *active-score*) '*this-track* ,@body) ))

;------ group -----------------------


(defmacro each-group (begin end &body body)
   `(cond 
          ((eq *rule-macro-context* nil)
           (each-object 
             (active-track-list *active-score*)
             '*this-track*
             (each-object 
               (track-list (make-group-meta-track (nth *i* *v*) :begin ,begin :end ,end)) 
               '*this-group* ,@body) ))
          ((eq *rule-macro-context* 'each-track)
           (each-object 
             (track-list (make-group-meta-track (nth *i* *v*) :begin ,begin :end ,end))
             '*this-group* ,@body) )
          ))

(defmacro each-group-if (begin end &body body)
   `(cond 
          ((eq *rule-macro-context* nil)
           (each-object (active-track-list *active-score*)  '*this-track*
             (each-object-if (track-list (make-group-meta-track (nth *i* *v*) :begin ,begin :end ,end))
               '*this-group* ,@body) ))
          ((eq *rule-macro-context* 'each-track)
           (each-object-if (track-list (make-group-meta-track (nth *i* *v*) :begin ,begin :end ,end))
             '*this-group* ,@body) )
          ))
   

;returns a meta-track containing a list of group sub-tracks
;begin and end statements are evaluated and the usual access functions are permitted
;assuming "this" as the position of the first and last segment, respectively
(defun make-group-meta-track (track &key begin end)
   (let ((meta-track (make-instance 'meta-track :trackname "group meta track"))
         (sub-track (make-instance 'basic-track)) )
     (print-ll "begin: " begin)
     (print-ll "end: " end)
      (each-object-if (segment-list track) '*this-segment*
        (eval begin)
        (then
          (let ((i-begin *i*))
            (block loop
              (loop for *i* from i-begin to (i?last) do
                    (add-one-segment sub-track (nth *i* *v*))
                    (if (eval end)
                      (progn
                        (add-one-track meta-track sub-track)
                        (setq sub-track (make-instance 'basic-track))
                        (return-from loop) ))))
            (setq *i* i-begin) )))
      meta-track ))


#| testing
(defun foo ()
   (each-group			            ;process phrase by phrase
   '(this 'phrase-start)			;group beginning
   '(this 'phrase-end)			;group end
   (then
     (print (this 'n))
     (set-this 'sl *i*) )))
(defun foo ()
   (each-group			            ;process phrase by phrase
   '(this 'phrase-start)			;group beginning
   '(or (last?) (next 'phrase-start))			;group end
   (then
     (print (this-dr))
     (set-this 'sl (- *i*)) )))

;nesting not possible
(defun foo ()
   (each-group			            ;process phrase by phrase
   '(this 'phrase-start)			;group beginning
   '(or (last?) (next 'phrase-start))			;group end
   (then
    (each-segment
     (print *i*)
     (add-this 'sl 0) ))))

|#
   
                  
;------ note-----------------------

(defmacro each-note (&body body)
  `(cond 
    ((eq *rule-macro-context* nil)
     (each-track
      (cond 
       ((or (typep (nth *i* *v*) 'mono-track)(typep (nth *i* *v*) 'midi-track))
        (each-object (segment-list (nth *i* *v*)) '*this-note* ,@body) ) 
       ((typep (nth *i* *v*) 'voice-track)
         (each-object (track-list (make-note-meta-track (nth *i* *v*))) '*this-note* ,@body)
         )))) 
    ((eq *rule-macro-context* 'each-track)
      (cond 
       ((or (typep (nth *i* *v*) 'mono-track)(typep (nth *i* *v*) 'midi-track))
        (each-object (segment-list (nth *i* *v*)) '*this-note* ,@body) ) 
       ((typep (nth *i* *v*) 'voice-track)
         (each-object (track-list (make-note-meta-track (nth *i* *v*))) '*this-note* ,@body)
        )))
     ))

(defmacro each-note-if (&body body)
  `(cond 
    ((eq *rule-macro-context* nil)
     (each-track
      (cond 
       ((or (typep (nth *i* *v*) 'mono-track)(typep (nth *i* *v*) 'midi-track))
        (each-object-if (segment-list (nth *i* *v*)) '*this-note* ,@body) ) 
       ((typep (nth *i* *v*) 'voice-track)
         (each-object-if (track-list (make-note-meta-track (nth *i* *v*))) '*this-note* ,@body)
         )))) 
    ((eq *rule-macro-context* 'each-track)
      (cond 
       ((or (typep (nth *i* *v*) 'mono-track)(typep (nth *i* *v*) 'midi-track))
        (each-object-if (segment-list (nth *i* *v*)) '*this-note* ,@body) ) 
       ((typep (nth *i* *v*) 'voice-track)
         (each-object-if (track-list (make-note-meta-track (nth *i* *v*))) '*this-note* ,@body)
        )))
     ))

;returns a meta-track containing a list of note sub-tracks
(defun make-note-meta-track (voice-track)
   (let ((meta-track (make-instance 'meta-track :trackname "note meta track"))
         (sub-track (make-instance 'basic-track)) )
      (each-object (segment-list voice-track) '*this-segment*
         (add-one-segment sub-track (nth *i* *v*))
         (if (last?)
            (add-one-track meta-track sub-track)
            (if (next 'n) 
               (progn
                  (add-one-track meta-track sub-track)
                  (setq sub-track (make-instance 'basic-track)) ))))
     ; (setf (meta-track-list voice-track)
     ;       (append (meta-track-list voice-track) (list meta-track)) )
      meta-track
      ))


;------ segment-----------------------

(defmacro each-segment (&body body)
   `(cond 
          ((eq *rule-macro-context* nil)
           (each-object (active-track-list *active-score*) '*this-track*
             (each-object (segment-list (nth *i* *v*)) '*this-segment* ,@body) ))
          ((eq *rule-macro-context* 'each-track)
           (each-object (segment-list (nth *i* *v*)) '*this-segment* ,@body) )
          ))

(defmacro each-segment-if (&body body)
   `(cond 
          ((eq *rule-macro-context* nil)
           (each-object (active-track-list *active-score*) '*this-track*
             (each-object-if (segment-list (nth *i* *v*)) '*this-segment* ,@body) ))
          ((eq *rule-macro-context* 'each-track)
           (each-object-if (segment-list (nth *i* *v*)) '*this-segment* ,@body) )
          ))




;; to be used within an ...-if macro        
(defmacro then (&body body)
 `(progn ,@body) )




      
;;----------------------------------------------------------
;;;----  access functions accessing track properties------
;;      to be used only within each-track macro
;;      only the slot names in a track are allowed
;;----------------------------------------------------------

(defun get-track-var (varname)
 (eval (list varname (nth *i* *v*)) ))

 (defun set-track-var (varname value)
 (eval (list 'setf (list varname (nth *i* *v*)) value)) )

(defun remove-this-track ()
   (remove-one-track *active-score* *i*)
   (setq *v* (active-track-list *active-score*))
   (decf *i*)
   (decf *v-length*) )
     
;;----------------------------------------------------------
;;;----  access functions accessing segment properties------
;;----------------------------------------------------------
      
;; helpfunctions, using *v* and *i* as "global" variables
;; *v* holds the list of objects
;; *i* holds the current object index (zero to length-1)
;; if nth gets a negative index it returns the first

      
;define also 
;,  multiply-this

;keywords: only-existing-values-p, offset, output list/mean/sum/first
; a version with keywords make the this-dr function unecessary

#|
(defun this (var &key (offset 0))
   (get-var (nth (+ *i* n) *v*) var) )

(defun set-this (var value &key (offset 0))
   (set-var (nth (+ *i* n) *v*) var value) )
|#

;; -------- get any property ---------------

(defun this (prop)
   (get-var (nth *i* *v*) prop))
(defun next (prop)
  (get-var (nth (1+ *i*) *v*) prop))
(defun prev (prop)
  (get-var (nth (1- *i* ) *v*) prop) )
(defun next2 (prop)
  (get-var (nth (+ *i* 2) *v*) prop) )
(defun prev2 (prop)
  (get-var (nth (- *i* 2) *v*) prop) )
(defun next3 (prop)
  (get-var (nth (+ *i* 3) *v*) prop) )
(defun prev3 (prop)
  (get-var (nth (- *i* 3) *v*) prop) )

;; -------- get duration  ---------------

(defun this-dr ()
   (get-dr (nth *i* *v*) ))
(defun next-dr ()
  (get-dr (nth (1+ *i*) *v*) ))
(defun prev-dr ()
  (get-dr (nth (1- *i* ) *v*) ) )
(defun next2-dr ()
  (get-dr (nth (+  2) *v*) ) )
(defun prev2-dr ()
  (get-dr (nth (- *i* 2) *v*) ) )
(defun next3-dr ()
  (get-dr (nth (+ *i* 3) *v*) ) )
(defun prev3-dr ()
  (get-dr (nth (- *i* 3) *v*) ) )


;; ----- set duration only -----


(defun set-this-dr (value)
   (set-dr (nth *i* *v*)  value)
   )
(defun set-next-dr (value)
   (set-dr (nth (1+ *i*) *v*)  value)
   )
(defun set-prev-dr (value) 
   (set-dr (nth (1- *i*) *v*)  value)
   )
(defun set-next2-dr (value)
   (set-dr (nth (+ *i* 2) *v*)  value)
   )
(defun set-prev2-dr (value) 
   (set-dr (nth (- *i* 2) *v*)  value)
   )
(defun set-next3-dr (value)
   (set-dr (nth (+ *i* 3) *v*)  value)
   )
(defun set-prev3-dr (value) 
   (set-dr (nth (- *i* 3) *v*)  value)
   )


;; --------- set any property ------------

;;;(defun set-this (prop value)
;;;   (set-var (nth *i* *v*) prop value)
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target 'current-note))
;;;   )      
;;;(defun set-next (prop value)
;;;   (set-var (nth (1+ *i*) *v*) prop value)
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target 'next-note))
;;;   )
;;;(defun set-prev (prop value) 
;;;   (set-var (nth (1- *i*) *v*) prop value)
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target 'previous-note))
;;;   )
;;;(defun set-next2 (prop value)
;;;   (set-var (nth (+ *i* 2) *v*) prop value)
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '2-notes-ahead))
;;;   )
;;;(defun set-prev2 (prop value) 
;;;   (set-var (nth (- *i* 2) *v*) prop value)
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '2-notes-back))
;;;   )
;;;(defun set-next3 (prop value)
;;;   (set-var (nth (+ *i* 3) *v*) prop value)
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '3-notes-ahead))
;;;   )
;;;(defun set-prev3 (prop value) 
;;;   (set-var (nth (- *i* 3) *v*) prop value)
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '3-notes-back))
;;;   )

(defun set-this (prop value)
  (set-var (nth *i* *v*) prop value) )

(defun set-next (prop value)
  (set-var (nth (1+ *i*) *v*) prop value) )

(defun set-prev (prop value) 
  (set-var (nth (1- *i*) *v*) prop value) )

(defun set-next2 (prop value)
  (set-var (nth (+ *i* 2) *v*) prop value) )

(defun set-prev2 (prop value) 
  (set-var (nth (- *i* 2) *v*) prop value) )

(defun set-next3 (prop value)
  (set-var (nth (+ *i* 3) *v*) prop value) )

(defun set-prev3 (prop value) 
  (set-var (nth (- *i* 3) *v*) prop value) )



;; ----------- add to the previous value of any property ------

;;;(defun add-this (prop value)
;;;   (set-var (nth *i* *v*) prop (+ (this prop) value))
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'add :target 'current-note))
;;;   )
;;;(defun add-next (prop value)
;;;   (set-var (nth (1+ *i*) *v*) prop (+ (next prop) value))
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target 'next-note))
;;;   )
;;;(defun add-prev (prop value)
;;;   (set-var (nth (1- *i*) *v*) prop (+ (prev prop) value))
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target 'previous-note))
;;;   )
;;;(defun add-next2 (prop value)
;;;   (set-var (nth (+ *i* 2) *v*) prop (+ (next2 prop) value))
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '2-notes-ahead))
;;;   )
;;;(defun add-prev2 (prop value)
;;;   (set-var (nth (- *i* 2) *v*) prop (+ (prev2 prop) value))
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '2-notes-back))
;;;   )
;;;(defun add-next3 (prop value)
;;;   (set-var (nth (+ *i* 3) *v*) prop (+ (next3 prop) value))
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '3-notes-ahead))
;;;   )
;;;(defun add-prev3 (prop value)
;;;   (set-var (nth (- *i* 3) *v*) prop (+ (prev3 prop) value))
;;;   (when (get-dm-var 'log-requested)
;;;      (fill-in-log *i* prop value :action 'set :target '3-notes-back))
;;;   )

(defun add-this (prop value)
  (set-var (nth *i* *v*) prop (+ (this prop) value)) )

(defun add-next (prop value)
  (set-var (nth (1+ *i*) *v*) prop (+ (next prop) value)) )

(defun add-prev (prop value)
  (set-var (nth (1- *i*) *v*) prop (+ (prev prop) value)) )

(defun add-next2 (prop value)
  (set-var (nth (+ *i* 2) *v*) prop (+ (next2 prop) value)) )

(defun add-prev2 (prop value)
  (set-var (nth (- *i* 2) *v*) prop (+ (prev2 prop) value)) )

(defun add-next3 (prop value)
  (set-var (nth (+ *i* 3) *v*) prop (+ (next3 prop) value)) )

(defun add-prev3 (prop value)
  (set-var (nth (- *i* 3) *v*) prop (+ (prev3 prop) value)) )


;; --------- multiply duration ------------

(defun multiply-this-dr (factor)
   (multiply-dr (nth *i* *v*)  factor))
(defun multiply-next-dr (factor)
   (multiply-dr (nth (1+ *i*) *v*)  factor))
(defun multiply-prev-dr (factor) 
   (multiply-dr (nth (1- *i*) *v*)  factor))
(defun multiply-next2-dr (factor)
   (multiply-dr (nth (+ *i* 2) *v*)  factor))
(defun multiply-prev2-dr (factor) 
   (multiply-dr (nth (- *i* 2) *v*)  factor))
(defun multiply-next3-dr (factor)
   (multiply-dr (nth (+ *i* 3) *v*)  factor))
(defun multiply-prev3-dr (factor) 
   (multiply-dr (nth (- *i* 3) *v*)  factor))

;; remove

(defun rem-this (prop)
   (rem-var (nth *i* *v*) prop))
(defun rem-next (prop)
  (rem-var (nth (1+ *i*) *v*) prop))
(defun rem-prev (prop)
  (rem-var (nth (1- *i* ) *v*) prop) )
(defun rem-next2 (prop)
  (rem-var (nth (+ *i* 2) *v*) prop) )
(defun rem-prev2 (prop)
  (rem-var (nth (- *i* 2) *v*) prop) )
(defun rem-next3 (prop)
  (rem-var (nth (+ *i* 3) *v*) prop) )
(defun rem-prev3 (prop)
  (rem-var (nth (- *i* 3) *v*) prop) )


;; ------- remove and insert segment --------

;;; Use these functions with caution using simple contexts as
;;; they destructively modify the segment-list
;;; e.g. (each-segment-if (first?) (remove-this-segment))
;;; will remove ALL segments

(defun remove-this-segment ()
   (remove-one-segment *this-track* *i*)
   (setq *v* (segment-list *this-track*))
   (decf *i*)
   (decf *v-length*) )

(defun insert-segment-before-this (segment)
   (add-one-segment-before-index *this-track*  *i* segment)
   (setq *v* (segment-list *this-track*))
   (incf *v-length*)
   (incf *i*)
   ) 

;; ------------ accessing any note ------------

(defun iget (i prop)
   (get-var (nth i *v*) prop))

(defun iset (i prop value)
  (set-var (nth i *v*) prop value))

;; system function in ACL
;(defun irem (i prop)
;  (rem-var (nth i *v*) prop))
 
(defun iadd (i prop addnr)
   (iset i prop (+ addnr (iget i prop))) )


;;f0
;;if f0 is a list (chord) it returns only the first
(defun this-f0 ()
   (let ((f0 (this 'f0)))
     (if (listp f0) (car f0) f0) ))

(defun next-f0 ()
   (let ((f0 (next 'f0)))
     (if (listp f0) (car f0) f0) ))
(defun prev-f0 ()
   (let ((f0 (prev 'f0)))
     (if (listp f0) (car f0) f0) ))

(defun next2-f0 ()
   (let ((f0 (next2 'f0)))
     (if (listp f0) (car f0) f0) ))
(defun prev2-f0 ()
   (let ((f0 (prev2 'f0)))
     (if (listp f0) (car f0) f0) ))

(defun next3-f0 ()
   (let ((f0 (next3 'f0)))
     (if (listp f0) (car f0) f0) ))
(defun prev3-f0 ()
   (let ((f0 (prev3 'f0)))
     (if (listp f0) (car f0) f0) ))

(defun iget-f0 (i)
   (let ((f0 (iget i 'f0)))
     (if (listp f0) (car f0) f0) ))


;;------- some other useful functions --------------------------


(defun last? () (= *i* (i?last)))

(defun first? () (= *i* 0))

;;;(defun first+1? ()
;;;  (if (= *i* 1) t nil) )
(defun first+1? () (= *i* 1))

;;;(defun last-1? ()
;;;  (if (= (1- (i?last)) *i*) t nil) )
(defun last-1? () (= (1- (i?last)) *i*))

;returns the number of the note with property prop after note i
;if not found ->nil
(defun i?next (i prop) 
  (untilexit end
    (cond ((>= i (1- (length *v*)))
           (return-from end nil))
          ((iget (incf i) prop)
           (return-from end i))
           )))

;returns the number of the note with property prop before note i
;if not found ->nil
(defun i?prev (i prop)
  (untilexit end
    (cond ((= i 0)
           (return-from end nil))
          ((iget (decf i) prop)
           (return-from end i))
           )))

;returns the number of the last note
(defun i?last ()
 (1- (length *v*)) )

;go to the end of the voice
;is not working?
(defun exit-track ()
  (setq *i* (i?last)))
  
#|
;get next note excluding rests
(defun i?next-note (i)
  (block last
        (let ((j (1+ i)))
          (until (not (iget j 'rest)) ;get j for next note
                 (let ((*i* j))
                    (if (last?) (return-from last nil)))
                 (incf j) )
          j )))

;get previous note excluding rests
(defun i?prev-note (i)
  (block last
        (let ((j (1- i)))
          (until (not (iget j 'rest)) ;get j for next note
                 (let ((*i* j))
                    (if (first?) (return-from last nil)))
                 (decf j) )
          j )))
|#

;get next note excluding rests
;not found -> nil
(defun i?next-note (i)
  (untilexit end
    (incf i)
    (cond ((>= i (1- (length *v*)))
           (return-from end nil))
          ((and (iget i 'n) (car (iget i 'n)) (not (iget i 'rest)))
           (return-from end i)
           ))))

;get previous note excluding rests
;not found -> nil
(defun i?prev-note (i)
  (untilexit end
    (decf i)
    (cond ((< i 0)
           (return-from end nil))
          ((and (iget i 'n) (car (iget i 'n)) (not (iget i 'rest)))
           (return-from end i)
           ))))


(defun sharp? ()
 (let ((tone (note-to-tone (this 'n))))
  (if (and (> (length tone) 1)(char= #\# (char tone 1))) t nil) ))

(defun flat? ()
 (let ((tone (note-to-tone (this 'n))))
  (if (and (> (length tone) 1)(char= #\b (char tone 1))) t nil) ))


;print the prop with additional info
;str will be printed first
;nil values will not be printed
(defun print-this (str prop bar)
  (terpri)
  (if str (prin1-ll str))
  (if prop (prin1-ll " " (string prop) " = " (this prop)))
  (if bar (prin1-ll " bar = " bar))
  (prin1-ll " *i* = " *i* " note = " (this 'n))
  )


;returns the index of the note up to drsum
;included the first note
;or next note if drsum > notesum
;returns at least i+1
;if it gets to the end - return last
(defun i?drsum (i drsum)
 (let ((dri 0.0))
  (untilexit end
     (cond ((not (iget i 'dr))                 ;last note?
            (return-from end (- i 1)) )
           (t (setq dri (+ dri (iget i 'dr)))  ;else
              (if (>= dri drsum)
                  (return-from end i) )
              (setq i (+ i 1)) )
         ))))

;add a value to the prop NOT included the last note
(defun iadd-val (i-from i-to val prop)
  (loop for i from i-from to (1- i-to) do
       (iset i prop (+ (iget i prop) val)) ))



;returns the the sum of the durations (float) not included i-to
(defun ndrsum (i-from i-to)
  (let ((drsum 0.0))
    (loop for i from i-from to (1- i-to) do
         (incf drsum (iget i 'ndr)) )
    drsum))

(defun drsum (i-from i-to)
  (let ((drsum 0.0))
    (loop for i from i-from to (1- i-to) do
         (incf drsum (iget i 'dr)) )
    drsum))

(defun drmean ()
  (/ (drsum 0 (i?last)) 
     (1+ (i?last)) ))

(defun ndrmean ()
  (/ (ndrsum 0 (i?last)) 
     (1+ (i?last)) ))


;;;---------------------------
;;; ramp functions
;;; should not be used in the future
;;; ---------------------------


;set the tones related to the durations to get a ramp
;that is added to the old values
;NOT included the last one
(defun iramp-add (i-from i-to addnr-from addnr-to prop)
  (let ((drdist (drsum i-from i-to))
        (maxstep (- addnr-to addnr-from)) )
       (loop for i from i-from to (1- i-to) do
         (if (not (iget i 'rest))
           (iset i  prop (round (+ (iget i prop)
                             addnr-from
                             (/ (* maxstep (drsum i-from i))
                                drdist) ))
                   ) ))))

(defun iramp-new (i-from i-to addnr-from addnr-to prop)
  (let ((drdist (drsum i-from i-to))
        (maxstep (- addnr-to addnr-from)) )
       (loop for i from i-from to (1- i-to) do
         (if (not (iget i 'rest))
           (iset i prop (round (+ addnr-from
                             (/ (* maxstep (drsum i-from i))
                                drdist) ))
                    ) ))))

(defun iramp-new-decimal (i-from i-to addnr-from addnr-to prop)
  (let ((drdist (drsum i-from i-to))
        (maxstep (- addnr-to addnr-from)) )
       (loop for i from i-from to (1- i-to) do
         (if (not (iget i 'rest))
           (iset i prop (+ addnr-from
                             (/ (* maxstep (drsum i-from i))
                                drdist) )
                    ) ))))

;take the ramp value from the middle of each note
(defun iramp-mean-new (i-from i-to addnr-from addnr-to prop)
  (let ((drdist (drsum i-from i-to))
        (maxstep (- addnr-to addnr-from)) )
       (loop for i from i-from to (1- i-to)do
         (if (not (iget i 'rest))
           (iset i  prop (round
                     (/ (+
                          (+ addnr-from
                             (/ (* maxstep (drsum i-from i))
                                drdist) )
                          (+ addnr-from
                             (/ (* maxstep (drsum i-from (1+ i)))
                                drdist) ))
                        2 ))
                   )))))

(defun iramp-mean-new-decimal (i-from i-to addnr-from addnr-to prop)
  (let ((drdist (drsum i-from i-to))
        (maxstep (- addnr-to addnr-from)) )
       (loop for i from i-from to (1- i-to) do
         (if (not (iget i 'rest))
           (iset i  prop
                     (/ (+
                          (+ addnr-from
                             (/ (* maxstep (drsum i-from i))
                                drdist) )
                          (+ addnr-from
                             (/ (* maxstep (drsum i-from (1+ i)))
                                drdist) ))
                        2 )
                   )))))

;take the ramp value from the middle of each note
; use cosine interpolation
(defun iramp-cos-mean-new (i-from i-to sval eval prop)
  (let ((etime (drsum i-from i-to)))
       (loop for i from i-from to (1- i-to) do
        (let ((time (+ (drsum i-from i) (/ (this 'dr) 2.))))
         (if (not (iget i 'rest))
           (iset i  prop
             (round
               (+ (/ (+ sval eval) 2.)
                  (* (/ (- sval eval) 2.)
                     (cos (/ (* time pi) etime)) ))))
           )))))

;;with decimal number output
(defun iramp-cos-mean-new-decimal (i-from i-to sval eval prop)
  (let ((etime (drsum i-from i-to)))
       (loop for i from i-from to (1- i-to) do
        (let ((time (+ (drsum i-from i) (/ (this 'dr) 2.))))
         (if (not (iget i 'rest))
           (iset i  prop
               (+ (/ (+ sval eval) 2.)
                  (* (/ (- sval eval) 2.)
                     (cos (/ (* time pi) etime)) )))
           )))))
(defun iramp-cos-new-decimal (i-from i-to sval eval prop)
  (let ((etime (drsum i-from i-to)))
       (loop for i from i-from to (1- i-to) do
        (let ((time (drsum i-from i)))
         (if (not (iget i 'rest))
           (iset i  prop
               (+ (/ (+ sval eval) 2.)
                  (* (/ (- sval eval) 2.)
                     (cos (/ (* time pi) etime)) )))
           )))))

(defun iramp-sqrt-new-decimal (i-from i-to sval eval prop)
  (let ((etime (drsum i-from i-to)))
    (cond
     ((<= sval eval)
       (loop for i from i-from to (1- i-to) do
        (let ((time (drsum i-from i)))
         (if (not (iget i 'rest))
           (iset i  prop
               (+ sval
                  (* (- eval sval)
                     (sqrt (/ time etime)) )))))))
     ((> sval eval)
       (loop for i from (1- i-to) downto i-from by 1 do
        (let ((time (drsum i i-to)))
         (if (not (iget i 'rest))
           (iset i  prop
               (+ eval
                  (* (- sval eval)
                     (sqrt (/ time etime)) )))))))
     )))


;(defun foo () (each-note-if (first?)(iramp-sqrt-new-decimal 0 10 -10 -50 'da0)))
;(defun foo () (each-note-if (first?)(iramp-mean-new 1 40 100 64 'a0)))


;interpolate between v1 och v2 depending on the duration 
;dr1 longer durationen
;v1  corresp. value
;dr2 shorter durationen
;v2  corresp. value
(defun dr-linear (curdr dr1 v1 dr2 v2)
   (infix ((v1 - v2) * curdr / (dr1 - dr2)
           + v2
           - (v1 - v2) * dr2 / (dr1 - dr2) )))

;(defun foo (dr) (dr-linear dr 100. 500. 10. 0.))    

;;as above with limits
(defun dr-linear-limits (curdr dr1 v1 dr2 v2)
  (let ((result
          (infix ((v1 - v2) * curdr / (dr1 - dr2)
                + v2
                - (v1 - v2) * dr2 / (dr1 - dr2) ))))
       (if (< curdr dr1) (setq result v1))
       (if (> curdr dr2) (setq result v2))
       result ))


;eof

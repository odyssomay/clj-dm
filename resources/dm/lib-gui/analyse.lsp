;;;-*-Mode: LISP; Package: DM -*-
;;
;; **********************************************
;;   a new tool for Analysing scores properties
;; **********************************************
;;
;; it's called from the menu
;;
;; 9605 /vc prototype
;; 970x /vc in progress

(in-package :dm)

;;
 ; CALL-TO-SET-SEGMENTS
 ;
 ; this function is called when a new track is selected
 ; it gets the track and call for the compilation of fields
(defun :call-to-set-segments (widget new old)
    (set-fields new))

;;
 ; CALL-TO-SET-COMPONENTS
 ;
 ; this function is called when a new component is selected
 ; it gets the track and call for the compilation of fields
 ;;
(defun :call-to-set-components (widget new old)
    (set-fields new))
    


;; AHHHHHHHHHHHHH !!

(defun give-me-track-name (foo)
    (trackname (first (track-list *active-score*))))


    

;;===================================================================
 ;  DEFGENERIC ANALYSE
 ;  it analyses any given musical object
 ;  it prepares and shows the dialog 
 ;  There's for sure a better way to use a generic
 ;
 ;   preliminary definition, with limited use of object capabilities
;;===================================================================


(defgeneric analyse (x))

(defmethod analyse ((x score))
   (ana)
   (setf *active-score* x)
   (setf (range (widget :lstSegment :ana)) ())   ;; because it's always dirty
   (set-dialog-field :ana :txtFilename (namestring (score-filename x)))
   (set-dialog-field :ana :txtObject 'score)
   (set-dialog-field :ana :txtNickname (nickname x))
   (set-fields x)
)


;;; 
;; there should be a func like this to get the class name ! ***** TYPE-OF
;; (object-name x)))

(defmethod analyse ((x track))
   (ana)
   (set-dialog-item-value :txtObject 'track)
   (set-dialog-item-value :lstTrack (list x))
   
)
                                     
(defmethod analyse ((x segment))
   (ana)
   (set-dialog-item-value :txtObject 'segment)
)  




;;===================================================================
 ;  DEFGENERIC SET-FIELDS
 ;
 ;  it compiles the fields on analyse window
 ;;===================================================================   
   
        
(defgeneric set-fields (x))

(defmethod set-fields ((x score))
   (set-dialog-item-range (widget :lstTrack :ana) (track-list x))
)

(defmethod set-fields ((x track))
   (set-dialog-field :ana :txtTrack (type-of x))
   (set-dialog-field :ana :txt-track-chan (midi-channel x))
   (set-dialog-field :ana :txt-track-vol (midi-initial-volume x))
   (set-dialog-field :ana :txt-track-synth (synth x))
   (set-dialog-item-range (widget :lstSegment :ana) (segment-list x))
   )

(defmethod set-fields ((x segment))
   (set-dialog-item-range (widget :lstComponent :ana) (var-list x))
)


;;
 ; :SHOW_PROPERTIES
 ; 
 ; it's called by a button and operates on the current track
 ; you could write a more general call to a window showing the
 ; properties, it can be used in several context into the program
 ;;
(defun :show-properties (widget new old)
   (let ((tr (dialog-field :ana :lstTrack)))
      (cond ((null tr)(advice "Please select a track"))
            (t (let (ans1 ans2 ans3) (multiple-value-setq (ans1 ans2 ans3)
                                       (pop-up-string-dialog *screen* "Analyse: ask for property" 
                                         "Please write the property that will be inspected in the track"
                                         question-icon "dr" "ok" "cancel"))
                  (cond ((= ans3 2)(advice "Request aborted by user"))
                        (t (each-segment (print (this ans1))))))))))
                
                
                
;;;                   (inspect ans)
;;;                   (cond ((null ans)(advice "Request aborted by user"))
;;;                       (t (each-segment (self )tr 'dr))))))))
            
   
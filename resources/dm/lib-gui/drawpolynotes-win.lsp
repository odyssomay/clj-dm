;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************
;;   display the notes on the screen
;; ****************************************

;; 1987 /Anders Friberg. 
;; 9201 CL /af
;; 9703 Win ACL version /Vittorio Colombo
;; 9810 converted to ACL5/af
;; 110418/af added right-click edit menu for notes
;; 110628/af added accent dialog to right-click menu in score window


(in-package :dm)

;;   draw-music
;;   redraw-music-windows


;(set-dm-var 'prop-list '(bind q :punct :weight))
;(set-dm-var 'prop-list '(bind phrase-start phrase-end))


; (prepare (make-music-window))



;; =====================================
;;  CLASS DEFINITIONS FOR MUSIC_WINDOW
;;                        SCROLLEE
;; =====================================
;;
(defclass music-window (dialog)
    ((my-scroll-bar :accessor my-scroll-bar)
     (my-scrollee :accessor my-scrollee) ))
    
(defclass scrollee (non-refreshing-pane)     
  ((ymin :initarg :ymin :initform 0 :accessor ymin) 
   (xmin :initarg :xmin :initform 0 :accessor xmin) 
   (yzero :initarg :yzero :initform 100 :accessor yzero)
   (xstart :initarg :xstart :initform 65 :accessor xstart) ;pix pos where the notes start
   (drabs-start :initarg :drabs-start :initform 0 :accessor drabs-start) ;the abs duration to start the plot

   ;selected-vars is a list of score variables that will be shown under the music
   (selected-vars :initarg :selected-vars :initform (get-dm-var 'prop-list)  :accessor selected-vars)

   ;the following five slots is only used for the popup dialog
   (selected-notation-vars :initarg :selected-notation-vars :initform (get-dm-var 'prop-list)  :accessor selected-notation-vars)
   (selected-performance-vars :initarg :selected-performance-vars :initform '()  :accessor selected-performance-vars)
   (selected-own-var-1 :initarg :selected-own-var-1 :initform ""  :accessor selected-own-var-1)
   (selected-own-var-2 :initarg :selected-own-var-2 :initform ""  :accessor selected-own-var-2)
   (selected-own-var-3 :initarg :selected-own-var-3 :initform ""  :accessor selected-own-var-3)
  
   (x-scale :initarg :x-scale :initform 30 :accessor x-scale) ;pix = dr / x-scale
   ;(x-page-increment :initarg :x-page-increment :initform 1000 :accessor x-page-increment) ; in drabs
   (xaxis-ndr? :initarg :xaxis-ndr? :initform t :accessor xaxis-ndr?) ;use ndr if t else dr
   (y-spacing :initarg :y-spacing :initform 70 :accessor y-spacing) ;spacing between music systems 
   ;(toclip? :initarg :toclip? :initform nil :accessor toclip?) 
   ))

;the duration for the music shown on the screen
(defmethod get-scrollee-absdr ((self scrollee))
   (* (- (interior-width self) (xstart self)) (x-scale self)) )


;; =====================================
;;  CLASS DEFINITIONS SCROLLBAR
;; =====================================

   
(defclass my-scroll-bar (horizontal-scroll-bar)
    ((view-container :initform nil :accessor view-container))
   )   

;; ---------------------
;;   MAKE-MUSIC-WINDOW
;; ---------------------
;;

(defun make-music-window (&key (parent *dm-main-window*) 
                           (window-interior (make-box (- (width (screen *system*)) 700)
                                              66 (- (width (screen *system*)) 4) 453)) 
                           (name 'music-window) 
                           (title "Score"))
   (let ((window-0 
           (make-window name
             :device 'music-window 
             :parent parent 
             :title title 
             :font (make-font :swiss :system 16 '(:bold)) 
             :window-state :shrunk 
             :window-border :frame 
             :left-attachment :left
             :top-attachment :top
             :right-attachment :right
             :bottom-attachment :bottom 
             :user-movable t 
             :user-resizable t 
             :user-closable t 
             :user-shrinkable t 
             :user-scrollable nil 
             :overlapped *overlapped-windows* 
             :background-color #S(rgb :red 227 :green 181 :blue 102); ljusgul(make-rgb :red 255 :green 255 :blue 208)
             :pop-up-p nil 
             :window-interior window-interior
             :widgets
             (list 
               (make-instance 'button 
                 :name :button-redraw 
                 :title "Redraw"
                 :box (make-box-relative 576 362 60 20) ;(make-box 741 362 798 382) 
                 :tabstop nil 
                 :groupstart nil 
                 :left-attachment :left 
                 :top-attachment :bottom 
                 :right-attachment :left 
                 :bottom-attachment :bottom 
                 :set-value-fn #'(lambda (widget old new)
                                   (let ((scrollee (my-scrollee (parent widget))))
                                      (redisplay-window scrollee)) )
                 :tooltip "Redraw the window"
                 :font (make-font-ex :swiss :arial 12 nil))
               (make-instance 'static-text 
                 :name :text-zoom-1 
                 :title "Static Text 23" 
                 :value "x-axis"
                 :box (make-box 155 362 197 378) 
                 :tabstop nil 
                 :groupstart t 
                 :top-attachment :bottom 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 15 nil))
               (make-instance 'button
                 :available-p t 
                 :name :button-zoom-x1 
                 :title "+"
                 :box (make-box 199 362 221 381) 
                 :tabstop nil 
                 :groupstart nil 
                 :top-attachment :bottom 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 15 '(:bold))
                 :tooltip "Increase note distance"
                 :set-value-fn #'(lambda (widget old new)
                                   (let ((scrollee (my-scrollee (parent widget))))
                                      (setf (x-scale scrollee) (+ (x-scale scrollee) -3))
                                      (if (< (x-scale scrollee) 2)
                                         (setf (x-scale scrollee) 2))
                                      (redisplay-window scrollee)
                                      ) t) )
               (make-instance 'static-text 
                 :name :text-zoom-2 
                 :title "Static Text 23" 
                 :value "y-axis"
                 :box (make-box 59 362 101 378) 
                 :tabstop nil 
                 :groupstart t 
                 :top-attachment :bottom 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 15 nil) 
                 :tab-control nil)
               (make-instance 'button
                 :available-p t  
                 :name :button-zoom-x2 
                 :title "-"
                 :box (make-box 222 362 243 381) 
                 :tabstop nil 
                 :groupstart nil 
                 :top-attachment :bottom 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 15 '(:bold)) 
                 :tab-control nil
                 :tooltip "Decrease note distance"
                 :set-value-fn #'(lambda (widget old new)
                                   (let ((scrollee (my-scrollee (parent widget))))
                                      (setf (x-scale scrollee) (+ (x-scale scrollee) 3))
                                      (if (> (x-scale scrollee) 300)
                                         (setf (x-scale scrollee) 300))
                                      (redisplay-window scrollee)
                                      ) t) )
               (make-instance 'button
                 :available-p t  
                 :name :button-zoom-y1 
                 :title "+" 
                 :box (make-box 101 362 123 381) 
                 :tabstop nil 
                 :groupstart nil 
                 :top-attachment :bottom 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 15 '(:bold)) 
                 :tab-control nil
                 :tooltip "Increase distance between music systems"
                 :set-value-fn #'(lambda (widget old new)
                                   (let ((scrollee (my-scrollee (parent widget))))
                                      (setf (y-spacing scrollee) (round (* (y-spacing scrollee) 1.2)))
                                      (redisplay-window scrollee)
                                      ) t) )
               (make-instance 'button 
                 :available-p t 
                 :name :button-zoom-y2 
                 :title "-"
                 :box (make-box 124 362 144 381) 
                 :tabstop nil 
                 :groupstart nil 
                 :top-attachment :bottom 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 15 '(:bold)) 
                 :tab-control nil
                 :tooltip "Decrease distance between music systems"
                 :set-value-fn #'(lambda (widget old new)
                                   (let ((scrollee (my-scrollee (parent widget))))
                                      (setf (y-spacing scrollee) (round (/ (y-spacing scrollee) 1.2)))
                                      (redisplay-window scrollee)
                                      ) t) )
               (make-instance 'button 
                 :name :ask-for-vars-button 
                 :title "Show Vars.." 
                 :box (make-box 270 362 370 382) 
                 :top-attachment :bottom
                 :bottom-attachment :bottom 

                 :font (make-font-ex :swiss :arial 13 nil) 
                 :tooltip "A dialog for selecting variables to be shown below each system"
                 :on-change #'(lambda (widget old new)
                                   (draw-ask-for-vars)
                                   (values t nil)  ) )
               (make-instance 'check-box 
                 :name :check-box-plot 
                 :title "x-axis: ndr (dr)" 
                 :value t 
                 :box (make-box 426 362 732 386) 
                 :tabstop nil 
                 :groupstart nil 
                 :top-attachment :bottom 
                 :right-attachment :right 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 13 nil) 
                 :tab-control nil
                 :tooltip "Horiz. axis shows nominal time if selected otherwise real time"
                 :set-value-fn #'(lambda (widget old new)
                                   (let ((scrollee (my-scrollee (parent widget))))
                                      (setf (xaxis-ndr? scrollee) (not new))
                                      (redisplay-window scrollee)
                                      (values t nil) ))  )
               (make-instance 'lisp-group-box 
                 :name :zoom-group-box 
                 :title "Zoom"
                 :box (make-box 6 352 249 386) 
                 :tabstop nil 
                 :groupstart nil 
                 :foreground-color blue 
                 :top-attachment :bottom 
                 :bottom-attachment :bottom 
                 :font (make-font-ex :swiss :arial 13 '(:italic)) 
                 :3d-style :raised-edge))
             )))
      (prepare window-0)     ; adding scroll bar widget and pane inside
      (show-window window-0 :normal)
      window-0)
   
   )
   
;; -------------------------
;;   MAKE-MY-SCROLL-BAR
;; -------------------------
;;
;; creates a my-scroll-bar object
;;
;; some defaults values are provided

(defun make-my-scroll-bar (&key (name 'my-scroll-bar)
                            (box nil)
                            (delayed t)
                            (range '(1 10000))
                            (value 1)
                            (view-container nil)
                            )
   (make-instance 'my-scroll-bar 
     :name name
     :box box
     :delayed delayed
     :direction :down 
     :groupstart nil
     :left-attachment :left
     :top-attachment :bottom
     :right-attachment :right
     :bottom-attachment :bottom 
     :background-color light-gray
     :page-increment 1000
     :range range
     :increment 1000
     :set-value-fn 'my-scroll-bar-click
     :tabstop t
     :title nil
     :value value
     :view-container view-container))



;; -----------------
;;   MAKE-SCROLLEE
;; -----------------
;;
;; creates a scrollee object
;;
;; some defaults values are provided

(defun make-scrollee (&key (parent *dm-main-window*)
                       (name 'scrollee)
                       (window-exterior nil)
                       view-container
                       )
   (let
    ((scrolle
      (open-stream
       'scrollee parent
       :output 
       :name name
       :background-color white
       :border :black        ; ***** why don't I get anything ??? Maybe... it does not exist !!
       :left-attachment :left
       :top-attachment :top
       :right-attachment :right
       :bottom-attachment :bottom 
       :user-scrollable nil        
       :window-exterior window-exterior
       :view-container view-container
       :tooltip "Double click on a note to edit variables"
       )))
    (setf (transparent-character-background scrolle) t)
    scrolle))
   



;; -----------------------
;;   MY-SCROLL-BAR-CLICK
;; -----------------------
;;
(defun my-scroll-bar-click (self new-value old-value)
   (when new-value
      (setf (drabs-start (my-scrollee (view-container self))) new-value)
      (redisplay-window (my-scrollee (view-container self)))
      ))


;; -----------------------------------
;;   Method PREPARE for music-window
;; -----------------------------------
;;
;; Initialize the music-window
;;
;; after creating a music-window, we add the my-scroll-bar
;; *apparently* (how ?) we cannot do this in the initialize-instance method for
;; music-window, because at that time the window is being created, and we cannot
;; add one widget. We have to do this later: that's different from Mac.
;; So right now, the initialize method for music-window is empty

(defmethod prepare ((self music-window))
   (add-component 
     (setf (my-scroll-bar self)
           (make-my-scroll-bar 
             :name 'my-scroll-bar
             :box (make-box-relative  6 (- (interior-height self) 50)
                       (- (interior-width self) 12) 15)
             :range (list 0 (round (get-dr (car (track-list *active-score*)))))
             :delayed t
             :view-container self))
     self)
   (setf (view-container (my-scroll-bar self)) self)  ;; ugly, but I'm not using make-instance...
   
   (setf (my-scrollee self)
         (make-scrollee
           :parent self
           :name 'my-scrollee
           :window-exterior (make-box-relative 6 5
                                 (- (interior-width self) 12)
                                 (- (interior-height self) 55))
           ))
   (setf (transparent-character-background (my-scrollee self)) t)
   )
  

;; --------------------------
;;   method REDISPLAY-MUSIC
;; --------------------------
;;
;; called when the window is resized
;;; 
;;; (defmethod redisplay-window :after ((self music-window) &optional box)
;;;    (if (slot-boundp self 'my-scrollee)  ; the slot is bound --> my-scrollee is defined
;;;       (progn      
;;;         (print "resize")
;;;         (clear-page (my-scrollee self))  ; ******* eliminate ?
;;;         (drawpn-draw-notes self)
;;;         )))

;;  --------------------------
;;    method REDISPLAY-MUSIC
;;  --------------------------
;;
;; called when the window is resized

(defmethod redisplay-window :after ((self scrollee) &optional box)
   (setf (page-increment (my-scroll-bar (parent self))) (- (get-scrollee-absdr self) 200))
   (setf (increment (my-scroll-bar (parent self)))
         (round (/ (page-increment (my-scroll-bar (parent self))) 10.0)) )
   (drawpn-draw-notes self)
)

;(defmethod redisplay-window :after ((self music-window) &optional box) )

;;  --------------------------
;;    mouse clicks
;;  --------------------------


(defmethod mouse-double-click ((self scrollee) buttons data)
   (let* ((drabs (+ (drabs-start self) 
                    (* (- (position-x data) (xstart self))
                       (x-scale self))))
          (track 
            (loop for i from 1 to (length (active-track-list *active-score*)) do
              (if (and (< (position-y data) (+ (* i (y-spacing self)) (/ (y-spacing self) 2.0)))
                       (> (position-y data) (- (* i (y-spacing self)) (/ (y-spacing self) 2.0))) )
                 (return (nth (1- i) (active-track-list *active-score*)))
                 nil)))
          (segment (if track (if (xaxis-ndr? self)
                                (get-segment-at-ndrabs track drabs)
                                (get-segment-at-drabs track drabs) )
                      nil))
          )
      
      (when segment ;(print (alist-to-list (var-list segment)))
         (draw-edit-segment-vars segment) )
     ))

#|
(defmethod mouse-right-down ((self scrollee) buttons data)
  (declare (ignore buttons))
  ;;compute which segment that was clicked
  (let* ((drabs (+ (drabs-start self) 
                   (* (- (position-x data) (xstart self))
                      (x-scale self))))
         (track 
          (loop for i from 1 to (length (active-track-list *active-score*)) do
                (if (and (< (position-y data) (+ (* i (y-spacing self)) (/ (y-spacing self) 2.0)))
                         (> (position-y data) (- (* i (y-spacing self)) (/ (y-spacing self) 2.0))) )
                    (return (nth (1- i) (active-track-list *active-score*)))
                  nil)))
         (segment (if track (if (xaxis-ndr? self)
                                (get-segment-at-ndrabs track drabs)
                              (get-segment-at-drabs track drabs) )
                    nil)))
    ;;make the popup menu
    (when segment
      (let* ((menu 
              (open-menu 
               (list 
                
                (make-instance 'menu-item :title "set phrase-start (4 5 6)" :value 'phst456)
                (make-instance 'menu-item :title "set phrase-start (5 6)" :value 'phst56)
                (make-instance 'menu-item :title "set phrase-start (6)" :value 'phst6)
                (make-instance 'menu-item :title "set accent perf" :value 'accentperf)
                (make-instance 'menu-item :title "set phrase-end (6)" :value 'phend6)
                (make-instance 'menu-item :title "set phrase-end (5 6)" :value 'phend56)
                (make-instance 'menu-item :title "set phrase-end (4 5 6)" :value 'phend456)
                (make-instance 'menu-item :title "remove phrase marks" :value 'remph)
                
                )
               'pop-up-menu (screen *system*)))
             answer)
        (unwind-protect
            (progn
              (setq answer (pop-up-menu menu (screen *system*) nil :left :center :right))
              (when answer
                (case answer
                  ('remph (progn (rem-var segment 'phrase-start) (rem-var segment 'phrase-end)))
                  ('phst456 (set-var segment 'phrase-start '(4 5 6)))
                  ('phst56 (set-var segment 'phrase-start '(5 6)))
                  ('phst6 (set-var segment 'phrase-start '(6)))
                  ('phend6 (set-var segment 'phrase-end '(6)))
                  ('phend56 (set-var segment 'phrase-end '(5 6)))
                  ('phend456 (set-var segment 'phrase-end '(4 5 6)))
                  ('accentperf (set-var segment 'phrase-end '(4 5 6)))
                  )
                ;(print answer)
                ))
          (close menu)
          (redisplay-window self)
          )))))
|#

;;with accent dialog
(defmethod mouse-right-down ((self scrollee) buttons data)
  (declare (ignore buttons))
  ;;compute which segment that was clicked
  (let (
        (drabs nil)
        (track nil)
        (track-nr nil)  ; track index
        (segment nil)
        (segment-nr nil) ) ;segment index
    (setq drabs (+ (drabs-start self) 
                   (* (- (position-x data) (xstart self))
                      (x-scale self))))
    (setq track 
          (loop for i from 1 to (length (active-track-list *active-score*)) do
                (when (and (< (position-y data) (+ (* i (y-spacing self)) (/ (y-spacing self) 2.0)))
                           (> (position-y data) (- (* i (y-spacing self)) (/ (y-spacing self) 2.0))) )
                  (setq track-nr (1- i))
                  (return (nth (1- i) (active-track-list *active-score*)))
                  nil)))
    ;(print-ll "track " track  "   track-nr   " track-nr)
    (if track (if (xaxis-ndr? self)
                  (multiple-value-setq (segment segment-nr) (get-segment-at-ndrabs track drabs))
                 (multiple-value-setq (segment segment-nr) (get-segment-at-drabs track drabs)) ))
    ;(print-ll "segment " segment  "   segment-nr   " segment-nr)
    ;;make the popup menu
    (when segment
      (let* ((menu 
              (open-menu 
               (list 
                (make-instance 'menu-item :title "set phrase-start (4 5 6)" :value 'phst456)
                (make-instance 'menu-item :title "set phrase-start (5 6)" :value 'phst56)
                (make-instance 'menu-item :title "set phrase-start (6)" :value 'phst6)
                (make-instance 'menu-item :title "set accent perf" :value 'accentperf)
                (make-instance 'menu-item :title "set phrase-end (6)" :value 'phend6)
                (make-instance 'menu-item :title "set phrase-end (5 6)" :value 'phend56)
                (make-instance 'menu-item :title "set phrase-end (4 5 6)" :value 'phend456)
                (make-instance 'menu-item :title "remove phrase marks" :value 'remph)
                )
               'pop-up-menu (screen *system*)))
             answer)
        (unwind-protect
            (progn
              (setq answer (pop-up-menu menu (screen *system*) nil :left :center :right))
              (when answer
                (case answer
                  ('remph (progn (rem-var segment 'phrase-start) (rem-var segment 'phrase-end)))
                  ('phst456 (set-var segment 'phrase-start '(4 5 6)))
                  ('phst56 (set-var segment 'phrase-start '(5 6)))
                  ('phst6 (set-var segment 'phrase-start '(6)))
                  ('phend6 (set-var segment 'phrase-end '(6)))
                  ('phend56 (set-var segment 'phrase-end '(5 6)))
                  ('phend456 (set-var segment 'phrase-end '(4 5 6)))
                  ('accentperf (edit-accents-dialog :track-number track-nr :note-number segment-nr))
                  )
                ;(print answer)
                ))
          (close menu)
          (redisplay-window self)
          )))))


;; -------------------------------
;;   method REDRAW-MUSIC-WINDOWS
;; -------------------------------
;;
;; called when music changed
;;
;;; (defun redraw-music-windows ()
;;;   (for window in (windows (windows (screen *system*))) 
;;;        when (typep window 'music-window)
;;;        all-satisfy
;;;        (redisplay-window window)))

(defun redraw-music-windows ()
  (dolist (window (windows (screen *system*))) 
       (when (typep window 'music-window)
          ;(print "redisplay")
          ;(redisplay-window window)
          (redisplay-window (my-scrollee window))
          (setf (range (my-scroll-bar window))
                (list 0 (round (get-dr (car (track-list *active-score*)))) ))
           ))
   )


;; ==================================
;; ===   T O P     L E V E L      ===
;; ==================================
;;
;; --------------
;;   DRAW-MUSIC
;; --------------
;;
(defun draw-music ()
   (if (find-window 'music-window *dm-main-window* :owned-p t)    ; if one or more 'music-window are existing
      (select-window (find-window 'music-window *dm-main-window* :owned-p t))      ; select one (the uppermost in the stack, probably)
      (make-music-window)                ; otherwise create one 
      ))


;; ---------------------
;;   DRAWPN-DRAW-NOTES
;; ---------------------
;;
;; ****** see what happens with with-focused-view
(defun drawpn-draw-notes (stream)
;  (with-focused-view view   ;******* what is it ?
    (gp-drawpn-draw-notes stream)
    
   )

;; ------------------------
;;   GP-DRAWPN-DRAW-NOTES
;; ------------------------
;;
;; called by drawpn-draw-notes
;;
;;; (defun gp-drawpn-draw-notes (stream)
;;;    (let ((yg (y-spacing stream))    ;current y pos of the note G
;;;          (sel-vars (selected-vars (my-scrollee (find-window 'music-window)))) )
;;;       (gp-move-to (+ (xmin stream) 1) (+ (ymin stream) 10) :stream stream)
;;;       (gp-select-text-font :stream stream)
;;;       (gp-write-string 
;;;          (concatenate 'string "Bar " (prin1-to-string (drabs-start stream))) :stream stream)
;;;       (each-track
;;;         (let  ((xcur (xmin stream))                    ;current x position
;;;                (bar-nr 0) )
;;;            (gp-select-text-font :stream stream)
;;;            (incf xcur 2)
;;;            (gp-move-to xcur (- yg 5) :stream stream)
;;;            (gp-write-string (get-track-var 'trackname) :stream stream)
;;;            (let ((dy 8)(y-start 6))
;;;               (dolist (var sel-vars)
;;;                  (gp-move-to xcur (+ yg (incf dy 10) y-start) :stream stream)
;;;                  (gp-write-string (princ-to-string var) :stream stream)) )
;;;            
;;;            (incf xcur 40)
;;;            (gp-select-music-font :stream stream)
;;;            (draw-music-lines xcur (interior-width stream) yg :stream stream)
;;;            (incf xcur 1)
;;;            ;(incf xcur (draw-bar xcur yg :stream stream))
;;;            (incf xcur (draw-g xcur yg :stream stream))
;;;            (incf xcur 2)
;;;            (each-note 
;;;              (if (this 'bar) (incf bar-nr))
;;;              (if (>= bar-nr (drabs-start stream))
;;;                 (then
;;;                   (let ((dx (/ (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)) (x-scale stream)))
;;;                         (prop-val-list (get-prop-val-list sel-vars)) )
;;;                      (if (iget *i* 'rest)
;;;                         (draw-rest (round xcur) yg (note-to-notevalue (this 'n))
;;;                           (this 'dot) prop-val-list :stream stream)
;;;                         (draw-note (round xcur) yg (this 'n) (this 'dot) prop-val-list :stream stream) )
;;;                      (incf xcur dx)
;;;                      ;(print xcur)
;;;                      (cond ((and (not (last?)) (next 'bar))
;;;                             (draw-bar (round xcur) yg :stream stream)
;;;                             (incf xcur 2)
;;;                             ))
;;;                      (if (>= xcur (interior-width stream))
;;;                         (exit-track) )
;;;                      ))))       
;;;            (incf yg (y-spacing stream))
;;;            ))
;;;       )
;;;    )

#|
(defun gp-drawpn-draw-notes (stream)
   (let ((yg (y-spacing stream))    ;current y pos of the note G
         (sel-vars (selected-vars (my-scrollee (find-window 'music-window *dm-main-window*)))) )
      (gp-move-to (+ (xmin stream) 1) (+ (ymin stream) 10) :stream stream)
      (gp-select-text-font :stream stream)
      (gp-write-string 
        (concatenate 'string "Start time (ms) " (prin1-to-string (drabs-start stream))) :stream stream)
      (each-track
       (let  ((xcur (xmin stream))                    ;current x position
              (drabs 0) (first-note? t) )
          (gp-select-text-font :stream stream)
          (incf xcur 2)
          (gp-move-to xcur (- yg 5) :stream stream)
          (gp-write-string (get-track-var 'trackname) :stream stream) ;print trackname
          (let ((dy 8))  ;print var names
             (dolist (var sel-vars)
                (gp-move-to xcur (+ yg (incf dy 10) 6) :stream stream)
                (gp-write-string (princ-to-string var) :stream stream)) )
          
          (incf xcur 40)
          (gp-select-music-font :stream stream)
          (draw-music-lines xcur (interior-width stream) yg :stream stream)
          (incf xcur 1)
          ;(incf xcur (draw-bar xcur yg :stream stream))
          (incf xcur (draw-g xcur yg :stream stream))
          (setf xcur (xstart stream))
          ;(print xcur)
          (each-segment
           (let ((dr (if (xaxis-ndr? stream)(this 'ndr)(this 'dr))))
              (if (>= drabs (drabs-start stream)) ;drabs-start is now abs dr
                 (then
                  (when first-note? 
                     (incf xcur (/ (- drabs (drabs-start stream))  (x-scale stream)))
                     (setq first-note? nil) )
                  (if (this 'rest)
                     (draw-rest (round xcur) yg (note-to-notevalue (this 'n))
                       (this 'dot) (get-prop-val-list sel-vars) :stream stream)
                     (draw-note (round xcur) yg (this 'n)
                       (this 'dot) (get-prop-val-list sel-vars) :stream stream) )
                  (incf xcur (/ dr (x-scale stream)))
                  ;(print xcur)
                  (cond ((and (not (last?)) (next 'bar))
                         (draw-bar (- (round xcur) 2) yg :stream stream)
                         ;(incf xcur 2) ;***not exact dr
                         ))
                  (if (>= xcur (interior-width stream)) (exit-track))
                  ))
              (incf drabs dr)
              ))       
          (incf yg (y-spacing stream))
         ))))

(defun gp-drawpn-draw-notes (stream)
   (let ((yg (y-spacing stream))    ;current y pos of the note G
         (sel-vars (selected-vars (my-scrollee (find-window 'music-window *dm-main-window*)))) )
      (gp-move-to (+ (xmin stream) 1) (+ (ymin stream) 10) :stream stream)
      (gp-select-text-font :stream stream)
      (gp-write-string 
        (concatenate 'string "Start time (ms) " (prin1-to-string (drabs-start stream))) :stream stream)
      (each-track
       (let  ((xcur (xmin stream))                    ;current x position
              (drabs 0) (first-note? t) )
          (gp-select-text-font :stream stream)
          (incf xcur 2)
          (gp-move-to xcur (- yg 5) :stream stream)
          (gp-write-string (get-track-var 'trackname) :stream stream) ;print trackname
          (let ((dy 8))  ;print var names
             (dolist (var sel-vars)
                (gp-move-to xcur (+ yg (incf dy 10) 6) :stream stream)
                (gp-write-string (princ-to-string var) :stream stream)) )
          
          (incf xcur 40)
          (gp-select-music-font :stream stream)
          (draw-music-lines xcur (interior-width stream) yg :stream stream)
          (incf xcur 1)
          ;(incf xcur (draw-bar xcur yg :stream stream))
          (incf xcur (draw-g xcur yg :stream stream))
          (setf xcur (xstart stream))
          ;(print xcur)
          (each-segment
           (let ((dr (if (xaxis-ndr? stream)(this 'ndr)(this 'dr))))
              (if (>= drabs (drabs-start stream)) ;drabs-start is now abs dr
                 (then
                  (when first-note? 
                     (incf xcur (/ (- drabs (drabs-start stream))  (x-scale stream)))
                     (setq first-note? nil) )
                  (if (this 'rest)
                     (draw-rest (round xcur) yg (note-to-notevalue (this 'n))
                       (this 'dot) (get-prop-val-list sel-vars) :tuple (or (this 'tuple) (this 't)) :stream stream)
                     (draw-note (round xcur) yg (this 'n)
                       (this 'dot) (get-prop-val-list sel-vars) :tuple (or (this 'tuple) (this 't)) :stream stream) )
                  (incf xcur (/ dr (x-scale stream)))
                  ;(print xcur)
                  (cond ((and (not (last?)) (next 'bar))
                         (draw-bar (- (round xcur) 2) yg :stream stream)
                         ;(incf xcur 2) ;***not exact dr
                         ))
                  (if (>= xcur (interior-width stream)) (exit-track))
                  ))
              (incf drabs dr)
              ))       
          (incf yg (y-spacing stream))
         ))))
|#

;;090406/af added  :owned-p t to find-window (new in 8.1)
(defun gp-drawpn-draw-notes (stream)
   (let ((yg (y-spacing stream))    ;current y pos of the note G
         (sel-vars (selected-vars (my-scrollee (find-window 'music-window *dm-main-window* :owned-p t)))) )
      (gp-move-to (+ (xmin stream) 1) (+ (ymin stream) 10) :stream stream)
      (gp-select-text-font :stream stream)
      (gp-write-string 
        (concatenate 'string "Start time (ms) " (prin1-to-string (drabs-start stream))) :stream stream)
      (each-track
       (let  ((xcur (xmin stream))                    ;current x position
              (drabs 0) (first-note? t) )
          (gp-select-text-font :stream stream)
          (incf xcur 2)
          (gp-move-to xcur (- yg 5) :stream stream)
          (gp-write-string (get-track-var 'trackname) :stream stream) ;print trackname
          (let ((dy 8))  ;print var names
             (dolist (var sel-vars)
                (gp-move-to xcur (+ yg (incf dy 10) 6) :stream stream)
                (gp-write-string (princ-to-string var) :stream stream)) )
          
          (incf xcur 40)
          (gp-select-music-font :stream stream)
          (draw-music-lines xcur (interior-width stream) yg :stream stream)
          (incf xcur 1)
          ;(incf xcur (draw-bar xcur yg :stream stream))
          (incf xcur (draw-g xcur yg :stream stream))
          (setf xcur (xstart stream))
          ;(print xcur)
          (each-segment
           (let ((dr (if (xaxis-ndr? stream)(this 'ndr)(this 'dr))))
              (if (>= drabs (drabs-start stream)) ;drabs-start is now abs dr
                 (then
                  (when first-note? 
                     (incf xcur (/ (- drabs (drabs-start stream))  (x-scale stream)))
                     (setq first-note? nil) )
                  (cond
                   ((and (this 'rest) (this 'n))
                    (draw-rest (round xcur) yg (note-to-notevalue (this 'n)) (this 'dot)
                               (get-prop-val-list sel-vars) :tuple (or (this 'tuple) (this 't)) :stream stream ))
                   ((this 'n)
                    (draw-note (round xcur) yg (this 'n) (this 'dot)
                               (get-prop-val-list sel-vars) :tuple (or (this 'tuple) (this 't)) :stream stream) )
                   ((this 'f0)
                    (draw-roll-bar (round xcur) (round (/ dr (x-scale stream))) yg (this 'f0)
                                   (get-prop-val-list sel-vars) :stream stream) )
                   (t ;rest and no note
                    (draw-rest-roll-bar (round xcur) (round (/ dr (x-scale stream))) yg
                               (get-prop-val-list sel-vars) :stream stream) ))
                  (incf xcur (/ dr (x-scale stream)))
                  ;(print xcur)
                  (cond ((and (not (last?)) (next 'bar))
                         (draw-bar (- (round xcur) 2) yg :stream stream)
                         ;(incf xcur 2) ;***not exact dr
                         ))
                  (if (>= xcur (interior-width stream)) (exit-track))
                  ))
              (incf drabs dr)
              ))       
          (incf yg (y-spacing stream))
          ))))

;; ---------------------
;;   GET-PROP-VAL-LIST
;; ---------------------
;; 
(defun get-prop-val-list ()
  (let ((propl (get-dm-var 'prop-list)) (val-list ()))
    (dolist (prop propl)
      (if (equal (this prop) 't)
        (newr val-list  prop)
        (newr val-list (this prop)) ))
    ;(print val-list)
    val-list ))

(defun get-prop-val-list (sel-vars)
   (let ((val-list ()))
    (dolist (prop sel-vars)
      (if (equal (this prop) 't)
        (newr val-list  prop)
         (if (floatp (this prop))
            (newr val-list (/ (round (* 100.0 (this prop))) 100.0)) ;show only 2 decimals
            (newr val-list (this prop)) )))
    ;(print val-list)
    val-list ))


;;;-*-Mode: LISP; Package: DM -*-
;;
;; **************************************************
;;   display the graph of a parameter on the screen
;;   PART 1 definitions of window objects
;; **************************************************
;;
;; 86-87.
;; 9112 CL /Anders Friberg 
;; 9704 ACL Win version /Vittorio Colombo
;; 980317   ??/af
;; 9810 converted to ACL5/af
;; 110223/af only one track displayed at a time with a selector


;; =================================================================================
;;    CLASSES DEFINITIONS FOR   DISPLAY-WINDOW DISPLAY-SCROLL-BAR DISPLAY_SCROLLEE 
;; =================================================================================

(defclass display-window (dialog) 
  ((display-scroll-bar :accessor display-scroll-bar)
   (display-scrollee :accessor display-scrollee) ))

(defclass display-scroll-bar (horizontal-scroll-bar)
    ((view-container :initform nil :accessor view-container))
) 

(defclass display-scrollee (non-refreshing-pane)
  ((ymin :initarg :ymin :initform 0 :accessor ymin) 
   (xmin :initarg :xmin :initform 0 :accessor xmin) 
   (ymax :initarg :ymax :initform 300 :accessor ymax) 
   (xmax :initarg :xmax :initform 300 :accessor xmax) 
   (ymax-controls :initarg :ymax-controls :initform (- 300 15) :accessor ymax-controls) 
   (ymax-notes :initarg :ymax-notes :initform (- 300 120) :accessor ymax-notes) 
   (ymax-graf :initarg :ymax-graf :initform (- 300 120) :accessor ymax-graf) 
   (yzero :initarg :yzero :initform 100 :accessor yzero) 
   (start-bar :initarg :start-bar :initform 1 :accessor start-bar)
   (start-track :initarg :start-track :initform 1 :accessor start-track) ;new 1102
   (drawprop-fn :initarg :drawprop-fn :accessor drawprop-fn)
   (x-scale :initarg :x-scale :initform 30 :accessor x-scale) 
   (y-scale :initarg :y-scale :initform 1 :accessor y-scale) 
   (y-name :initarg :y-name :accessor y-name) 
   (xstart :initarg :xstart :initform 30 :accessor xstart) 
   (square-plot? :initarg :square-plot? :initform nil :accessor square-plot?) 
   (xaxis-ndr? :initarg :xaxis-ndr? :initform t :accessor xaxis-ndr?) 
   (toclip? :initarg :toclip? :initform nil :accessor toclip?) 
   (notes? :initarg :notes? :initform t :accessor notes?)

   ))

;; -------------------------
;;   MAKE-DISPLAY-WINDOW
;; -------------------------
;;

(defvar display-window nil)  ;???

(defun make-display-window (&key (parent *dm-main-window*) 
                                 ;(window-interior (make-box 170 56 979 443)) 
                                 (window-interior (make-box 
                                                   4 
                                                   (- (height (screen *system*)) 399)
                                                   (- (width (screen *system*)) 20)
                                                   (- (height (screen *system*)) 10))) 
                                 ;(window-interior (make-box 
                                 ;                   0 (- (height (screen *system*)) 250)
                                 ;                   (width (screen *system*))
                                 ;                   (height (screen *system*)))) 
                                 (name :display-window) 
                                 (title "please-define-me"))
  (let ((window-0
         (make-window name
           :device 'display-window
           :parent parent 
           :title title 
           :font (make-font :swiss :system 16 '(:bold)) 
           :window-state :shrunk 
           :window-border :frame 
           :left-attachment nil 
           :top-attachment nil 
           :right-attachment nil 
           :bottom-attachment nil 
           :user-movable t 
           :user-resizable t 
           :user-closable t 
           :user-shrinkable t 
           :user-scrollable nil 
           :overlapped *overlapped-windows* 
           :background-color #S(rgb :red 239 :green 221 :blue 165) ; ljusgul (make-rgb :red 255 :green 255 :blue 208) 
           :pop-up-p nil 
           :window-interior window-interior
           ;:window-exterior window-exterior
           :widgets
           (list 
            ;--track select---
            (make-instance 'combo-box
              :name :track
              :title "starting track" 
              :box (make-box-relative 5 362 50 16) 
              ;:left 526
              ;:top 362
              :on-change #'(lambda (widget old new)
                             (let ((scrollee (display-scrollee (parent widget))))
                               (setf (start-track scrollee) old) ;skumt men funkar
                               ;(print-ll "old " old " new " new)
                               (redisplay-window scrollee)
                               ) t)

              :range '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
              :value 1
              :tooltip "Starting track to display"
              :width 50
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :right-attachment :left
              :left-attachment :left
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 13 nil) 
              :tab-control nil
              )
            (make-instance 'static-text 
              :name :text-track 
              :title "text track"
              :value "Track"
              :box (make-box-relative 58 365 50 16) 
              :tabstop nil 
              :groupstart t 
              :top-attachment :bottom 
              :right-attachment :left
              :left-attachment :left
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 13 nil))

            ;---zoom-----
;;;            (make-instance 'lisp-group-box 
;;;              :name :zoom-group-box 
;;;              :title "Zoom"
;;;              :box (make-box 6 352 249 386) 
;;;              :tabstop nil 
;;;              :groupstart nil 
;;;              :foreground-color blue 
;;;              :top-attachment :bottom 
;;;              :bottom-attachment :bottom 
;;;              :font (make-font-ex :swiss :arial 13 '(:italic)) 
;;;              :3d-style :raised-edge)
            (make-instance 'button
              :available-p t  
              :name :button-zoom-y1 
              :title "+" 
              :box (make-box 111 364 133 383) 
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 15 '(:bold)) 
              :tab-control nil
              :tooltip "Increase distance between music systems"
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (setf (y-scale scrollee) (/ (y-scale scrollee) 2.0))
                                  (redisplay-window scrollee)
                                  ) t) )
            (make-instance 'button 
              :available-p t 
              :name :button-zoom-y2 
              :title "-"
              :box (make-box 134 364 154 383) 
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 15 '(:bold)) 
              :tab-control nil
              :tooltip "Decrease distance between music systems"
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (setf (y-scale scrollee) (* (y-scale scrollee) 2.0))
                                  (redisplay-window scrollee)
                                  ) t) )
            (make-instance 'static-text 
              :name :text-zoom-2 
              :title "Static Text 23"
              :value "y-axis"
              :box (make-box 158 364 201 380) 
              :tabstop nil 
              :groupstart t 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 15 nil) 
              :tab-control nil)
            (make-instance 'button
              :available-p t 
              :name :button-zoom-x1 
              :title "+"
              :box (make-box 209 364 231 383) 
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :tooltip "Increase note distance"
              :font (make-font-ex :swiss :arial 15 '(:bold))
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (setf (x-scale scrollee) (+ (x-scale scrollee) -3))
                                  (if (< (x-scale scrollee) 2)
                                      (setf (x-scale scrollee) 2))
                                  (redisplay-window scrollee)
                                  ) t) )
            
            (make-instance 'button
              :available-p t  
              :name :button-zoom-x2 
              :title "-"
              :box (make-box 232 364 253 383) 
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 15 '(:bold)) 
              :tooltip "Decrease note distance"
              :tab-control nil
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (setf (x-scale scrollee) (+ (x-scale scrollee) 3))
                                  (if (> (x-scale scrollee) 300)
                                      (setf (x-scale scrollee) 300))
                                  (redisplay-window scrollee)
                                  ) t) )
            (make-instance 'static-text 
              :name :text-zoom-1 
              :title "Static Text 23"
              :value "x-axis"
              :box (make-box 255 364 307 380) 
              :tabstop nil 
              :groupstart t 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 15 nil))

            ;---check boxes-----           
             (make-instance 'check-box 
              :available-p t 
              :name :check-box-square 
              :title "Square plot"
              :box (make-box 309 362 399 386) 
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 13 nil)
              :tooltip "Show the value as a horizontal line over each note"
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (setf (square-plot? scrollee) (not new) )
                                  (redisplay-window scrollee)
                                  (values t nil) ))  )
            (make-instance 'check-box 
              :available-p t 
              :name :check-box-notes 
              :title "Notes"
              :value t 
              :box (make-box 409 362 464 386) 
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 13 nil) 
              :tab-control nil
              :tooltip "Draw the music system"
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (setf (notes? scrollee) (not new))
                                  (adjust-x-and-y-max scrollee)
                                  (redisplay-window scrollee)
                                  (values t nil) ))  )
            (make-instance 'check-box 
              :name :check-box-plot 
              :title "x-axis: ndr (dr)" 
              :value t 
              :box (make-box-relative 476 362 120 24) 
              :tabstop nil 
              :groupstart nil 
              :top-attachment :bottom 
              :right-attachment :left
              :left-attachment :left
              :bottom-attachment :bottom 
              :font (make-font-ex :swiss :arial 13 nil) 
              :tab-control nil
              :tooltip "Horiz. axis shows nominal time if selected otherwise real time"
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (setf (xaxis-ndr? scrollee) (not new))
                                  (redisplay-window scrollee)
                                  (values t nil) ))  )
            
            (make-instance 'button 
              :name :button-redraw 
              :title "Redraw" 
              :box (make-box-relative 600 362 60 20) 
              :tabstop nil 
              :groupstart nil 
              :left-attachment :left 
              :top-attachment :bottom 
              :right-attachment :left 
              :bottom-attachment :bottom 
              :set-value-fn #'(lambda (widget old new)
                                (let ((scrollee (display-scrollee (parent widget))))
                                  (adjust-x-and-y-max scrollee)
                                  (redisplay-window scrollee)) )
              :tooltip "Redraw the window"
              :font (make-font-ex :swiss :arial 12 nil))
            
            )
           )))
    (show-window window-0 nil)
    window-0))

;; -------------------------
;;   MAKE-DISPLAY-SCROLLEE
;; -------------------------
;;
;; creates a display-scrollee object
;;
;; some defaults values are provided
;;
(defun make-display-scrollee (&key (parent *dm-main-window*)
                               (name 'display-scrollee)
                               (window-exterior nil)
                               )
   (let ((scrolle
          (open-stream
           'display-scrollee parent
           :output 
           :name name
           :background-color white
           :left-attachment :left
           :top-attachment :top
           :right-attachment :right
           :bottom-attachment :bottom 
           :user-scrollable nil        
           :window-exterior window-exterior
           )))
      (setf (transparent-character-background scrolle) t)
      scrolle))

;; -------------------------
;;   MAKE-DISPLAY-SCROLL-BAR
;; -------------------------
;;
;; creates a display-scroll-bar object
;; some defaults values are provided
;;
(defun make-display-scroll-bar (&key (name 'display-scroll-bar)
                            (box (make-box 6 333 798 349))
                            (delayed t)
                            (range (list 1 50))
                                 )
   (make-instance 'display-scroll-bar 
     :name name
     :box box
     :delayed delayed
     :direction :down 
     :groupstart nil
     :left-attachment :left
     :top-attachment :bottom
     :right-attachment :right
     :bottom-attachment :bottom 
     :page-increment 5
     :range range
     :set-value-fn 'display-scroll-bar-click
     :background-color light-gray
     :tabstop t
     :title nil
     :value 1))

 
;; ----------------------------
;;   DISPLAY-SCROLL-BAR-CLICK
;; ----------------------------
;;
(defun display-scroll-bar-click (self new-value old-value)
   (when new-value
      (let ((win (display-scrollee (view-container self))))
         (setf (start-bar win) new-value)
         (redisplay-window win)
         ))
   )

;; -----------------------------------
;;   Defun PREPARE-display-window
;; -----------------------------------
;;
;; Create and initialize the display-window
;; it should be a method, maybe with make-instance...
;; I don't like in this way with a defun
;;
(defun prepare-display-window (&key (name "Provide a title, please")
                                (y-name "Provide a y-name")
                                (y-scale 1)
                                drawprop-fn)
   (let ((self (make-display-window :title name)))
      (add-component 
        (setf (display-scroll-bar self)
              (make-display-scroll-bar 
                :name 'display-scroll-bar
                :box (make-box-relative 6 (- (interior-height self) 51)
                       (- (interior-width self) 12) 15)))
        self)
      (setf (view-container (display-scroll-bar self)) self)  ;; ugly, but I'm not using make-instance...
      
      (setf (display-scrollee self)
            (make-display-scrollee
              :parent self
              :name 'display-scrollee
              :window-exterior (make-box-relative 6 5
                                 (- (interior-width self) 12)
                                 (- (interior-height self) 57))     ;**** shouldbe rel2scroll
              ))
      (setf (y-name (display-scrollee self)) y-name)
      (setf (y-scale (display-scrollee self)) y-scale)
      (setf (drawprop-fn (display-scrollee self)) drawprop-fn)
      (adjust-x-and-y-max (display-scrollee self))
      ;(setf (background-mode (display-scrollee self)) :transparent)
      (redisplay-window (display-scrollee self)) ;***** necessary ?
      ))


;; ---------------------------------
;;   method REDRAW-DISPLAY-WINDOWS
;; ---------------------------------
;;
;; called when music changed
;; you can fuse with the redraw-music-windows one


(defun redraw-display-windows ()
  (dolist (window (windows (screen *system*))) 
       (when (typep window 'display-window)
          (print "redisplay")
          ;(redisplay-window window)
           (redisplay-window (display-scrollee window))
           ))
   )


;; (defmethod window-zoom-event-handler ((self display-window) message)
;; unnecessary

;;; 
;;; (defmethod print-contents ((self display-window) &optional (offset #@(0 0)))
;;;   (call-next-method)
;;;   ;(view-draw-contents (display-scrollee self))
;;;   )


;;  -----------------------------------------------
;;    method REDISPLAY-WINDOW for display-scrollee
;;  -----------------------------------------------
;;
;; called when the window is resized
;; in Mac it's called view-draw-contents

(defmethod redisplay-window :after ((self display-scrollee) &optional box)
   (if (slot-boundp self 'drawprop-fn)
      (draw-prop :stream self)
      (print "redisplay action delayed, slot is unbound")
   ))

(defmethod resize-window ((self display-scrollee) pos)
   (if (slot-boundp self 'drawprop-fn)
      (progn 
        (adjust-x-and-y-max self)
        (erase-contents-box self
          (make-box 0 0 (interior-width self) 
                        (interior-height self)))
        (draw-prop :stream self) )
      (print "redisplay action delayed, slot is unbound")
   ))

;; ----------------------
;;   ADJUST-X-AND-Y-MAX
;; ----------------------
;;
(defmethod adjust-x-and-y-max ((self display-scrollee))
  (setf (xmax self) (interior-width self))
  (setf (ymax self) (interior-height self))
  (cond
   ((notes? self)
    (setf (ymax-graf self) (- (ymax self) 80))
    (setf (ymax-notes self) (- (ymax self) 80))
    (setf (ymax-controls self) (- (ymax self) 0))
    (setf (yzero self) (round (/ (ymax-graf self) 1.5))) )
   (t
    (setf (ymax-graf self) (- (ymax self) 0))
    (setf (ymax-notes self) (- (ymax self) 0))
    (setf (ymax-controls self) (- (ymax self) 0))
    (setf (yzero self) (+ 10 (round (/ (ymax-graf self) 1.5)))) )
   ))

;;; ------------------
;;;   PRINT-CONTENTS
;;; ------------------
;;; 
;;; sends to printer
;;; 
;;; (defmethod ccl::print-contents ((self display-scrollee) &optional (offset #@(0 0)))
;;;  (call-next-method)
;;;  (let ((get-dm-var 'to-printer?) t))
;;;    (ccl::scale-line-width 1/4)
;;;    (gp-draw-prop self)
;;;    (ccl::normal-line-width) ))


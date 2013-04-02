;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************
;;   display the notes on the screen
;; ****************************************
;;
;; 1987 /Anders Friberg. 
;; 9201 CL /af

(in-package :dm)

;;   draw-music
;;   redraw-music-windows

;(set-dm-var 'prop-list '(bind q :punct :weight))  ; ***** this was commented out
;(set-dm-var 'prop-list '(bind phrase-start phrase-end))

(require 'quickdraw)

(defclass music-window (window) 
  ((my-scroll-bar :accessor my-scroll-bar)
   (my-scrollee :accessor my-scrollee) ))

(defclass scrollee (simple-view)
  ((ymin :initarg :ymin :initform 0 :accessor ymin) 
   (xmin :initarg :xmin :initform 0 :accessor xmin) 
   (ymax :initarg :ymax :initform 300 :accessor ymax) 
   (xmax :initarg :xmax :initform 300 :accessor xmax) 
   (ymax-controls :initarg :ymax-controls :initform (- 300 15) :accessor ymax-controls) 
   (ymax-notes :initarg :ymax-notes :initform (- 300 120) :accessor ymax-notes) 
   (ymax-graf :initarg :ymax-graf :initform (- 300 120) :accessor ymax-graf) 
   (yzero :initarg :yzero :initform 100 :accessor yzero) 
   (start-bar :initarg :start-bar :initform 1 :accessor start-bar)
   (drawprop-fn :initarg :drawprop-fn :accessor drawprop-fn)
   (x-scale :initarg :x-scale :initform 30 :accessor x-scale) 
   (y-scale :initarg :y-scale :initform 1 :accessor y-scale) 
   (y-name :initarg :y-name :accessor y-name) 
   (xstart :initarg :xstart :initform 30 :accessor xstart) 
   (square-plot? :initarg :square-plot? :initform nil :accessor square-plot?) 
   (toclip? :initarg :toclip? :initform nil :accessor toclip?) 
   (notes? :initarg :notes? :initform t :accessor notes?) 
   ))


(defmethod initialize-instance ((self music-window) &key
                                (track-thumb-p nil))
    (call-next-method)
    (setf (my-scroll-bar self)
          (make-instance
            'scroll-bar-dialog-item
            :view-container self
            ;:view-size (make-point (- (point-h (view-size self)) 15) 15)
            :length (- (point-h (view-size self)) 15)
            :width 16
            :view-position (make-point 0 (- (point-v (view-size self)) 15))
            :track-thumb-p track-thumb-p
            :direction :horizontal
            :max 50
            :min 1
            :page-size 5
            :setting 1
            :dialog-item-action
            #'(lambda (self)
                (setf (start-bar (my-scrollee (view-container self)))
                      (scroll-bar-setting self))
                (view-draw-contents (view-container self)) )
            ))
    (setf (my-scrollee self)
          (make-instance
            'scrollee
            :view-container self
            :view-size (make-point (- (point-h (view-size self)) 2)
                                   (- (point-v (view-size self)) 15))
            ))
    (adjust-x-and-y-max (my-scrollee self)) )


(defmethod view-draw-contents ((self scrollee))
  (call-next-method)
    (drawpn-draw-notes self)
  )

(defmethod ccl::print-contents ((self scrollee) &optional (offset #@(0 0)))
  (call-next-method)
  (ccl::scale-line-width 1/4)
  (gp-drawpn-draw-notes self)
  (ccl::normal-line-width))

(defmethod set-view-size ((self music-window) h &optional v)
  (declare (ignore h v))
  (without-interrupts
   (call-next-method)
   (set-scroll-bar-length (my-scroll-bar self) (- (point-h (view-size self)) 15))
   (set-view-position (my-scroll-bar self) (make-point 0 (- (point-v (view-size self)) 15)))
   (set-view-size (my-scrollee self) 
                  (make-point (- (point-h (view-size self)) 2)
                              (- (point-v (view-size self)) 15)) )
   (adjust-x-and-y-max (my-scrollee self))
   ;(view-draw-contents self)
   ))

(defmethod adjust-x-and-y-max ((self scrollee))
  (setf (xmax self) (point-h (view-size self)))
    (setf (ymax self) (point-v (view-size self))) )

(defmethod window-zoom-event-handler ((self music-window) message)
  (declare (ignore message))
  (without-interrupts
   (call-next-method)
   (set-scroll-bar-length (my-scroll-bar self) (- (point-h (view-size self)) 15))
   (set-view-position (my-scroll-bar self) (make-point 0 (- (point-v (view-size self)) 15)))
   (set-view-size (my-scrollee self) 
                  (make-point (- (point-h (view-size self)) 2)
                              (- (point-v (view-size self)) 15)) )
   (adjust-x-and-y-max (my-scrollee self))
   ;(view-draw-contents self)
   ))

;; called when music changed
(defun redraw-music-windows ()
  (dolist (window (windows :class 'music-window))
    (view-draw-contents window) ))


;; top level ----------------------------------

(defun draw-music ()
  (if (find-window "Draw music")
    (window-select (find-window "Draw music"))
    (make-instance 'music-window
                   :window-title "Draw music"
                   :window-type :document-with-zoom
                   )))

(defun drawpn-draw-notes (view)
  (with-focused-view view
    (gp-drawpn-draw-notes view) ))

(defun gp-drawpn-draw-notes (view)
  (let ((yg (+ (ymin view) 80)))    ;current y pos of the note G
    (gp-erase-rect (xmin view) (ymin view) (xmax view) (ymax view))
    ;(print (clip-region view))
    ;(print (view-size view))
    (gp-move-to (+ (xmin view) 1) (+ (ymin view) 10))
    (gp-select-text-font)
    (gp-write-string (concatenate 'string "Bar " (prin1-to-string (start-bar view))))
    (each-track
      (let  ((xcur (xmin view))                    ;current x position
             (bar-nr 0) )
        (gp-select-music-font)
        (draw-music-lines (xmin view) (xmax view) yg)
        (incf xcur (draw-bar xcur yg))
        (incf xcur (draw-g xcur yg))
        (each-note 
          (if (this 'bar) (incf bar-nr))
          (if (>= bar-nr (start-bar view))
            (then
              (let ((dx (round (/ (this 'ndr) (x-scale view))))
                    (prop-val-list ()) )
                (if (get-dm-var 'prop-list)
                   (setq prop-val-list (get-prop-val-list)))
                (if (iget *i* 'rest)
                  (draw-rest xcur yg (note-to-notevalue (this 'n))
                             (this 'dot) prop-val-list)
                  (draw-note xcur yg (this 'n) (this 'dot) prop-val-list) )
                (incf xcur dx)
                (cond ((and (not (last?)) (next 'bar))
                       (draw-bar xcur yg)
                       (incf xcur 2)
                       ))
                (if (>= xcur (xmax view))
                  (exit-track) )
                ))))
        (incf yg 70) 
        ))))

(defun get-prop-val-list ()
  (let ((propl (get-dm-var 'prop-list)) (val-list ()))
    (dolist (prop propl)
      (if (equal (this prop) 't)
        (newr val-list  prop)
        (newr val-list (this prop)) ))
    ;(print val-list)
    val-list ))

;; eof

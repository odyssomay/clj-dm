
;-------display the graph of a parameter on the screen-------

;;86-87.
;; 9112 CL
;;/Anders Friberg 

(in-package :dm)

(require 'drawbasic)
(require 'quickdraw)
;(require 'picture-files)

(defvar *print-drm-p*)
(setq *print-drm-p* nil)
;(setq *print-drm-p* t)

(defvar *to-printer?* nil)

(defun get-initial-viewsize () (make-point (- *screen-width* 10) 250))

;; called when something else changed
(defun redraw-display-windows ()
  (dolist (window (windows :class 'display-window))
    (view-draw-contents window) ))




;;---- the window class ------------------------------------------


(defclass display-window (window) 
  ((my-scroll-bar :accessor my-scroll-bar)
   (my-scrollee :accessor my-scrollee) ))

(defmethod initialize-instance ((self display-window) &key
                                (track-thumb-p nil) y-name y-scale drawprop-fn)
  (call-next-method)
  ;(set-back-color self (make-color (* 257 239) (* 257 221) (* 257 165)))
  ;(set-part-color self :frame (make-color (* 257 239) (* 257 221) (* 257 165)))
  (add-subviews self
                (make-dialog-item 'button-dialog-item
                                  #@(0 0)
                                  #@(50 15)
                                  "Redraw"
                                  #'(lambda (item)
                                      item (view-draw-contents self))
                                  :view-font '("Geneva" 9 :plain))
                (make-dialog-item 'button-dialog-item
                                  #@(50 0)
                                  #@(15 15)
                                  "-"
                                  #'(lambda (item)
                                      item
                                      (setf (y-scale (my-scrollee self))
                                            (* (y-scale (my-scrollee self)) 2.0))
                                      (view-draw-contents self))
                                  :view-font '("Geneva" 9 :plain))
                (make-dialog-item 'button-dialog-item
                                  #@(65 0)
                                  #@(15 15)
                                  "+"
                                  #'(lambda (item)
                                      item
                                      (setf (y-scale (my-scrollee self))
                                            (/ (y-scale (my-scrollee self)) 2))
                                      (view-draw-contents self))
                                  :view-font '("Geneva" 9 :plain))
                (make-dialog-item 'button-dialog-item
                                  #@(80 0)
                                  #@(15 15)
                                  "><"
                                  #'(lambda (item)
                                      item
                                      (setf (x-scale (my-scrollee self))
                                            (+ (x-scale (my-scrollee self)) 3))
                                      (if
                                        (> (x-scale (my-scrollee self)) 300)
                                        (setf (x-scale (my-scrollee self)) 300))
                                      (view-draw-contents self))
                                  :view-font '("Geneva" 9 :plain))
                (make-dialog-item 'button-dialog-item
                                  #@(95 0)
                                  #@(15 15)
                                  "<>"
                                  #'(lambda (item)
                                      item
                                      (setf (x-scale (my-scrollee self))
                                            (+ (x-scale (my-scrollee self)) -3))
                                      (if (< (x-scale (my-scrollee self)) 2)
                                        (setf (x-scale (my-scrollee self)) 2))
                                      (view-draw-contents self))
                                  :view-font '("Geneva" 9 :plain))
                (make-dialog-item 'check-box-dialog-item
                                  #@(110 0)
                                  #@(80 15)
                                  "Square plot"
                                  #'(lambda (item)
                                      (if
                                        (check-box-checked-p item)
                                        (setf (square-plot? (my-scrollee self)) t)
                                        (setf (square-plot? (my-scrollee self)) nil))
                                      (view-draw-contents self))
                                  :view-font '("Geneva" 9 :plain))
         (make-dialog-item 'check-box-dialog-item
                           #@(190 0)
                           #@(50 15)
                           "Notes"
                           #'(lambda (item)
                                   (if (check-box-checked-p item)
                                     (setf (notes? (my-scrollee self)) t)
                                     (setf (notes? (my-scrollee self)) nil))
                                   (adjust-x-and-y-max (my-scrollee self))
                                   (view-draw-contents self))
                           :check-box-checked-p t
                           :view-font '("Geneva" 9 :plain))
         (make-dialog-item 'check-box-dialog-item
                           #@(250 0)
                           #@(100 15)
                           "xaxis: ndr (dr)"
                           #'(lambda (item)
                                   (if (check-box-checked-p item)
                                     (setf (xaxis-ndr? (my-scrollee self)) t)
                                     (setf (xaxis-ndr? (my-scrollee self)) nil))
                                   (view-draw-contents self))
                           :check-box-checked-p t
                           :view-font '("Geneva" 9 :plain))
                )
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
          'display-scrollee
          :view-container self
          :view-size (make-point (- (point-h (view-size self)) 2)
                                 (- (point-v (view-size self)) 30))
          :view-position (make-point 0 15)
          :y-name y-name
          :y-scale y-scale
          :drawprop-fn drawprop-fn
          ))
  (adjust-x-and-y-max (my-scrollee self)) )

(defmethod set-view-size ((self display-window) h &optional v)
  (declare (ignore h v))
  (invalidate-view (my-scrollee self))
  (without-interrupts
   (call-next-method)
   ;(print "HEJ")
   (set-scroll-bar-length (my-scroll-bar self) (- (point-h (view-size self)) 15))
   (set-view-position (my-scroll-bar self) (make-point 0 (- (point-v (view-size self)) 15)))
   (set-view-size (my-scrollee self) 
                  (make-point (- (point-h (view-size self)) 2)
                              (- (point-v (view-size self)) 30)) )
   (adjust-x-and-y-max (my-scrollee self))
   ))

(defmethod window-zoom-event-handler ((self display-window) message)
  (declare (ignore message))
  (invalidate-view (my-scrollee self))
  (without-interrupts
   (call-next-method)
   (set-scroll-bar-length (my-scroll-bar self) (- (point-h (view-size self)) 15))
   (set-view-position (my-scroll-bar self) (make-point 0 (- (point-v (view-size self)) 15)))
   (set-view-size (my-scrollee self) 
                  (make-point (- (point-h (view-size self)) 2)
                              (- (point-v (view-size self)) 30)) )
   (adjust-x-and-y-max (my-scrollee self))
   ))

;for now
(defmethod view-draw-contents ((self display-window))
  (call-next-method))

(defmethod ccl::print-contents ((self display-window) &optional (offset #@(0 0)))
  (call-next-method)
  ;(view-draw-contents (my-scrollee self))
  )


;;------------ scrollee class -------------------------------------------

(defclass display-scrollee (simple-view)
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
   (xaxis-ndr? :initarg :xaxis-ndr? :initform t :accessor xaxis-ndr?) 
   (toclip? :initarg :toclip? :initform nil :accessor toclip?) 
   (notes? :initarg :notes? :initform t :accessor notes?) 
   ))



(defmethod adjust-x-and-y-max ((self display-scrollee))
  (setf (xmax self) (point-h (view-size self)))
  (cond
   ((notes? self)
    (setf (ymax self) (point-v (view-size self)))
    (setf (ymax-graf self) (- (ymax self) 80))
    (setf (ymax-notes self) (- (ymax self) 80))
    (setf (ymax-controls self) (- (ymax self) 0))
    (setf (yzero self) (round (/ (ymax-graf self) 1.5))) )
   (t
    (setf (ymax self) (point-v (view-size self)))
    (setf (ymax-graf self) (- (ymax self) 0))
    (setf (ymax-notes self) (- (ymax self) 0))
    (setf (ymax-controls self) (- (ymax self) 0))
    (setf (yzero self) (+ 10 (round (/ (ymax-graf self) 1.5)))) )
   ))

(defmethod view-draw-contents ((self display-scrollee))
  (call-next-method)
    (draw-prop self)
  )
(defmethod ccl::print-contents ((self display-scrollee) &optional (offset #@(0 0)))
  (call-next-method)
  (let ((*to-printer?* t))
    (ccl::scale-line-width 1/4)
    (gp-draw-prop self)
    (ccl::normal-line-width) ))

;; ----- top level currently from the menu ------------------


(defun draw-level ()
  (if (find-window "ÆLevel")
    (window-select (find-window "ÆLevel"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "ÆLevel"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "ÆLevel [dB]"
                   :y-scale 0.1
                   :drawprop-fn 'sl-fn
                   )))
(defun draw-ddr ()
  (if (find-window "ÆDuration")
    (window-select (find-window "ÆDuration"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "ÆDuration"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "ÆDR  [ms]"
                   :y-scale 1
                   :drawprop-fn 'ddr-fn
                   )))

(defun draw-dr ()
  (if (find-window "Duration")
    (window-select (find-window "Duration"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Duration"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "DR  [ms]"
                   :y-scale 10
                   :drawprop-fn 'dr-fn
                   )))
(defun draw-bar-dr ()
  (if (find-window "Bar duration")
    (window-select (find-window "Bar duration"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Bar duration"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "DR  [ms]"
                   :y-scale 10
                   :drawprop-fn 'bar-dr-fn
                   )))

(defun draw-beat-dr ()
  (if (find-window "Beat duration")
    (window-select (find-window "Beat duration"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Beat duration"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "DR  [ms]"
                   :y-scale 10
                   :drawprop-fn 'beat-dr-fn
                   )))

(defun draw-beat-dr% ()
  (if (find-window "Beat duration (%)")
    (window-select (find-window "Beat duration (%)"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Beat duration (%)"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "ÆDR  [%]"
                   :y-scale 0.25
                   :drawprop-fn 'beat-dr%-fn
                   )))

(defun draw-ddr% ()
  (if (find-window "ÆDuration (%)")
    (window-select (find-window "ÆDuration (%)"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "ÆDuration (%)"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "ÆDR  [%]"
                   :y-scale 0.25
                   :drawprop-fn 'ddr%-fn
                   )))

(defun draw-tempo ()
  (if (find-window "ÆTempo")
    (window-select (find-window "ÆTempo"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "ÆTempo"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "Ætempo  [%]"
                   :y-scale 2
                   :drawprop-fn 'tempo-fn
                   )))

(defun draw-time-scoretime ()
  (if (find-window "Time - Scoretime")
    (window-select (find-window "Time - Scoretime"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Time - Scoretime"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "Time - Scoretime [ms]"
                   :y-scale 2
                   :drawprop-fn 'time-scoretime-fn
                   )))

(defun draw-droff ()
  (if (find-window "Offtime duration")
    (window-select (find-window "Offtime duration"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Offtime duration"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "DRO  [ms]"
                   :y-scale 1
                   :drawprop-fn 'droff-fn
                   )))

(defun draw-dro%IOI ()
  (if (find-window "Offtime duration")
    (window-select (find-window "Offtime duration"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Offtime duration as % of IOI"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "DRO [% of IOI]"
                   :y-scale 1
                   :drawprop-fn 'dro%IOI-fn
                   )))

(defun draw-f0-mean ()
  (if (find-window "f0 mean")
    (window-select (find-window "f0 mean"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "f0 mean"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "f0 from C4, mean of 5  [tonnr]"
                   :y-scale 1
                   :drawprop-fn 'f0-mean-fn
                   )))

(defun draw-dcent ()
  (if (find-window "ÆCent")
    (window-select (find-window "ÆCent"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "ÆCent"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "ÆF  [cent]"
                   :y-scale 1
                   :drawprop-fn 'dcent-fn
                   )))


(defun draw-va ()
  (if (find-window "Vibrato amp")
    (window-select (find-window "Vibrato amp"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Vibrato amp"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "va  [cent]"
                   :y-scale 0.25
                   :drawprop-fn 'va-fn
                   )))

(defun draw-vf ()
  (if (find-window "Vibrato freq")
    (window-select (find-window "Vibrato freq"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Vibrato freq"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "vf  [Hz]"
                   :y-scale 0.1
                   :drawprop-fn 'vf-fn
                   )))

(defun draw-envelope ()
  (if (find-window "Envelope amplitude")
    (window-select (find-window "Envelope amplitude"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Envelope amplitude"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "Envelope amplitude  [dB]"
                   :y-scale 0.5
                   :drawprop-fn 'env-fn
                  ; :yzero 40
                   )))

(defun draw-volume ()
  (if (find-window "Volume")
    (window-select (find-window "Volume"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "Volume"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name "Volume  [dB]"
                   :y-scale 0.5
                   :drawprop-fn 'vol-fn
                  ; :yzero 40
                   )))

(defun draw-mi ()
  (if (find-window "mi")
    (window-select (find-window "mi"))
    (make-instance 'display-window
                   :window-type :document-with-zoom
                   :window-title "mi"
                   :view-size (get-initial-viewsize)
                   :view-position '(:bottom 5)
                   :y-name " mi    "
                   :y-scale 1
                   :drawprop-fn 'mi-fn
                   )))

;;---- parameter access functions ----------------

;(defun da0-fn ()
;  (if (this 'da0)
;    (* 0.1 (this 'da0))
;    nil))

;(defun da0-fn ()
; (* 0.1 (or (this 'da0) 0)))

(defun sl-fn ()
 (or (this 'sl) 0))



(defun ddr-fn ()
 (- (this 'dr) (this 'ndr)))

(defun dr-fn ()
 (this 'dr))

(defun bar-dr-fn ()
  (if (and
       (this 'bar)
       (i?next *i* 'bar) )
    (drsum *i* (i?next *i* 'bar))
    0))

(defun beat-dr-fn ()
  (if (first?)
    (mark-beat-dr))
  (when (last?)         ;doesn't work -will not arrive there 
    (rem-all :beat-dr)
    (rem-all :beat))
  (if (this :beat-dr)
    (this :beat-dr) 0)
  )
(defun mark-beat-dr ()
  (let (beat-dr dr-ack)
    (each-note             ;mark beat
      (when (this 'meter) 
        (setq beat-dr (round (note-to-dr (cons nil (cadr (this 'meter)))))))
      (when (this 'bar) 
        (setq dr-ack 0))
      (when (or (this 'bar)(zerop (mod (round dr-ack) beat-dr)))
        (set-this :beat t)
        ;(print-ll *i* "  " (this 'n))
        )
      (setq dr-ack (+ dr-ack (nom-dr *i*)))
      )
    (each-note-if           ;compute the dr to next beat
      (this :beat)
      (i?next *i* :beat)
      (then
        (loop for i from *i* to (1- (i?next *i* :beat)) do
        (iset i :beat-dr (drsum *i* (i?next *i* :beat)))
        (iset i :beat-ndr (ndrsum *i* (i?next *i* :beat))) )
      ))))

(defun beat-dr%-fn ()
  (if (first?)
    (mark-beat-dr))
  (if (this :beat-dr)
    (* (/ (- (this :beat-dr) (this :beat-ndr)) (this :beat-ndr)) 100.)
    nil) )


(defun ddr%-fn ()
 (* (/ (- (this 'dr) (this 'ndr)) (this 'ndr)) 100.))

(defun tempo-fn ()
 (* (/ (this 'ndr)(this 'dr)) 100.))

(defun time-scoretime-fn ()
  (- (drsum 0 *i*) (ndrsum 0 *i*)) )

(defun droff-fn ()
 (if (this 'dro)(this 'dro) 0))

;; dro%IOI = dro/IOI
(defun dro%IOI-fn ()
 (if (this 'dro)(* (/ (this 'dro)(this 'ndr)) 100.0) 0))

(defun dcent-fn ()
 (if (this 'dc)(this 'dc)
        (let ((i *i*))
          (until (iget (decf i) 'dc))
          (iget i 'dc) ))
 )
(defun dcent-fn ()
 (this 'dc))


(defun va-fn ()
  (if (this 'va)
    (this 'va)
    nil))

(defun vf-fn ()
  (if (this 'vf)
    (this 'vf)
    nil))

(defun env-fn ()
  (env-to-tl-list (this 'env)))

(defun vol-fn ()
  (this 'vol))

(defun mi-fn ()
 (if (this 'mi) (float (this 'mi)) 0.))

(defun f0-mean-fn ()
 (if (this 'f0-mean) (- (float (this 'f0-mean)) 60) 0.))

;;--- draw the graph -----------------------------------

;draws the graph
;if propfn return nil there will not be any line on that note
;if propfn gives a value less than minimum it will be set to max-graf (min)
(defun draw-prop (view)
  (with-focused-view view
    (gp-draw-prop view) ))


(defun gp-draw-prop (view)
  (with-fore-color *blue-color*
  (let ((bar-nr 0)(xcur 4)(lastx 4)
        (lasty (yzero view))
        (yzero (yzero view))
        (propfn (drawprop-fn view))
        (start-bar (start-bar view)) )
    (gp-erase-rect (xmin view) (ymin view) 
                (point-h (view-size view)) (- (point-v (view-size view)) 0) )
    (with-fore-color *black-color*
      (gp-move-to 0 0)                   ;a line at the top
      (gp-line-to (point-h (view-size view)) 0) )
    (gp-select-text-font)
    (draw-y-axis view)
    (setq xcur (xstart view))
    (setq lastx (xstart view))

    (each-segment
     (if (this 'bar) (incf bar-nr))
     (if (>= bar-nr start-bar)
      (then
       (let ((dx (round (/ (if (xaxis-ndr? view)(this 'ndr)(this 'dr)) (x-scale view)))))
        (gp-move-to xcur (+ yzero 2))  ;x-axis
        (gp-line 0 -4)                 ;draw the vertical bar
        (gp-move 2 0)
        (if (this 'bar)                  ;draw the bar number
            (gp-write-string (prin1-to-string (this 'bar))) )
        (gp-move-to xcur yzero)        ;draw the x-axis part dotted
        (#_PenPat *gray-pattern*)
        (gp-line-to (+ xcur dx) yzero)
        (#_PenNormal)
        (cond
         ((and (eval (list propfn))             ;time envelope?
               (listp (eval (list propfn))))
          (gp-move-to xcur lasty)
          (setq lasty (draw-env-one-note view (eval (list propfn)) xcur yzero dx))
          (incf xcur dx) )
         ((square-plot? view)                   ;square plot?
          (gp-move-to xcur lasty)
          (ifn (call-fn view)
               (incf xcur dx)
               (progn
                 (gp-line-to  xcur (setq lasty (call-fn view)))   ;first vertical part
                 (incf xcur dx)
                 (gp-line-to xcur lasty) )))         ;horisontal part
         (t                                     ;else
          (gp-move-to lastx lasty)
          (if (call-fn view)
            (then
              (gp-line-to xcur (setq lasty (call-fn view)))
              (gp-paint-oval (- xcur 2)(- lasty 1)(+ xcur 2)(+ lasty 3)) )
            (gp-line-to xcur lasty) )
          (setq lastx xcur)
          (incf xcur dx) ))
        (if (> xcur (xmax view))
          (exit-track) )
        ))))))
  (if *print-drm-p* (draw-prop-drm view))
  (if (notes? view) (drawprop-notes view)) 
  )

(defun ddrm%-fn ()
  (if (this :drm)
    (* (/ (- (this :drm) (this 'ndr)) (this 'ndr)) 100.)
    nil ))


;draw a second graph above as ddr% but with the prop :drm
(defun draw-prop-drm (view)
  (let ((bar-nr 0)(xcur 4)(lastx 4)
        (lasty (yzero view))
        (yzero (yzero view))
        (propfn 'ddrm%-fn)
        (start-bar (start-bar view)) )
    (setq xcur (xstart view))
    (setq lastx (xstart view))

    (each-segment
     (if (this 'bar) (incf bar-nr))
     (if (>= bar-nr start-bar)
      (then
       (let ((dx (round (/ (if (xaxis-ndr? view)(this 'ndr)(this 'dr)) (x-scale view)))))
        (cond
         ((and (eval (list propfn))             ;time envelope?
               (listp (eval (list propfn))))
          (draw-env-one-note view (eval (list propfn)) xcur yzero dx)
          (incf xcur dx) )
         ((square-plot? view)                   ;square plot?
          (gp-move-to xcur lasty)
          (ifn (call-fn-drm view)
               (incf xcur dx)
               (progn
                 (gp-line-to xcur (setq lasty (call-fn-drm view)))   ;first vertical part
                 (incf xcur dx)
                 (gp-line-to xcur lasty) )))         ;horisontal part
         (t                                     ;else
          (gp-move-to lastx lasty)
          (if (call-fn-drm view)
            (then
              (with-fore-color *blue-color*
                ;(#_PenPat *gray-pattern*)
                (gp-line-to xcur (setq lasty (call-fn-drm view))))
              ;(#_PenNormal)
              ;(gp-paint-oval (- xcur 2)(- lasty 1)(+ xcur 2)(+ lasty 3))
              )
            (gp-line-to xcur lasty) )
          (setq lastx xcur)
          (incf xcur dx) ))
        (if (> xcur (xmax view))
          (exit-track) )
        )))))
  )

;;draw a time-level list of parameters for one note
;;returns the last y value
(defun draw-env-one-note (view tl-list x y dx)
  (let ((y-scale (slot-value view 'y-scale))
        (time)(level))
    (while tl-list
      (setq time (pop tl-list)) 
      (setq level (pop tl-list))
      (gp-line-to (round (+ x (* dx (/ time (if (xaxis-ndr? view)(this 'ndr)(this 'dr))))))
               (round (- y (/ level y-scale))) ))
    (when (< time (if (xaxis-ndr? view)(this 'ndr)(this 'dr)))
      (gp-line-to (round (+ x dx))
               (round (- y (/ level y-scale))) ))
    (round (- y (/ level y-scale)))
    ))

;used by draw-prop
;returns the new y value in mac-local units
;return nil when nil from propfn or if propfn returns a list
;without lower limit
(defun call-fn (view)
 (let ((val (eval (list (slot-value view 'drawprop-fn)))))
  (if (and val (not (listp val)))
       (let ((y (- (slot-value view 'yzero)
                   (round (/ val (slot-value view 'y-scale))))))
             y )
       nil )))

(defun call-fn-drm (view)
 (let ((val (ddrm%-fn)))
  (if (and val (not (listp val)))
       (let ((y (- (slot-value view 'yzero)
                   (round (/ val (slot-value view 'y-scale))))))
             y )
       nil )))

(defun draw-y-axis (view)
 (let ((yrel 0)
       (xstart (slot-value view 'xstart))
       (yzero (slot-value view 'yzero)))
  (gp-move-to xstart (slot-value view 'yzero))
  (loop for y from (- yzero 20) downto (+ (slot-value view 'ymin) 10)  by 20 do  ;the positive part
    (incf yrel 20)
    (gp-line-to xstart y)
    (gp-move -1 0)
    (gp-line 2 0)
    (let ((str (string-number (* yrel (slot-value view 'y-scale)))))
      (gp-move (- (+ (* (length str) 6) 3)) 3)
      (gp-write-string str) )
    (gp-move-to xstart y) )
  (gp-write-string (concatenate 'string "  " (slot-value view 'y-name)))

  (gp-move-to xstart yzero)           ;the negative part
  (setq yrel 0)
  (loop for y from (+ yzero 20)  to (slot-value view 'ymax-graf) by 20 do
    (incf yrel -20)
    (gp-line-to xstart y)
    (gp-move -1 0)
    (gp-line 2 0)
    (let ((str (string-number (* yrel (slot-value view 'y-scale)))))
      (gp-move (- (+ (* (length str) 6) 3)) 3)
      (gp-write-string str) )
    (gp-move-to xstart y) )
 ))
    
(defun string-number (nr)
  (cond ((integerp nr)
         (prin1-to-string nr) )
        ((zerop (- (truncate nr) nr))
         (prin1-to-string (truncate nr)) )
        (t "") ))


;;-------------- draw the notes ---------------------------------


;to be used with parameter display
(defun drawprop-notes (view)
 (let ((yg (round (+ (/ (- (slot-value view 'ymax-controls)      ;current y pos of the note G 
                      (slot-value view 'ymax-notes)) 2.0)
              (slot-value view 'ymax-notes))))
       (xcur (+ (slot-value view 'xmin) 10))                ;current x position
       ;(ymin (slot-value view 'ymax-notes))
       ;(ymax (slot-value view 'ymax-controls))
       (bar-nr 0) )
   (gp-select-music-font)
   (draw-music-lines xcur (slot-value view 'xmax) yg)
   (draw-bar xcur yg)
   (draw-g xcur yg)
   (setq xcur (slot-value view 'xstart))
   (each-segment 
     (if (this 'bar) (incf bar-nr))
     (if (>= bar-nr (slot-value view 'start-bar))
      (then
       (let ((dx (round (/ (if (xaxis-ndr? view)(this 'ndr)(this 'dr)) (slot-value view 'x-scale)))))
         (if (this 'rest)
             (draw-rest xcur yg (note-to-notevalue (this 'n)) (this 'dot) nil)
             (draw-note xcur 
                    (if *to-printer?* (- yg 1) yg) (this 'n) (this 'dot) nil ))
         (incf xcur dx)
         (cond ((and (not (last?)) (next 'bar))
                (draw-bar (- xcur 3) yg) ))
         (if (>= xcur (slot-value view 'xmax))
             (exit-track) )
      ))))))


;;---------- save to PICT file ------------------

(defmethod window-save-as ((w display-window))
  (let ((fpath (choose-new-file-dialog 
                :directory (merge-pathnames ".pict" (or (get-filename) "temp")))))
    (when fpath
      (with-cursor *watch-cursor*
        (with-focused-view w
          (with-pict-output-file (fpath w #@(0 0) (view-size w))
            (draw-prop (my-scrollee w)) ))
        (set-mac-file-type fpath "PICT")
        (set-mac-file-creator fpath "SPNT")
        ))))

;;(display-pict-file "hd:ekor.pict" 0.5)

;;; EOF

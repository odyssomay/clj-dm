;;;-*-Mode: LISP; Package: DM -*-
;;
;; **************************************************
;;   display the graph of a parameter on the screen
;;   PART 2
;; **************************************************
;;
;; 86-87.
;; 9112 CL /Anders Friberg 
;; 9704 ACL Win version /Vittorio Colombo
;; 20001006 Roberto Bresin added: draw-dro%IOI()
;; 110223/af only one track displayed
    
(in-package :dm)


;; ----- top level currently from the menu ------------------

(defun draw-level ()
   (prepare-display-window
     :name "Sound Level"
     :y-name "delta Sound Level [dB]"
     :y-scale 0.1
     :drawprop-fn 'sl-fn
     ))

(defun draw-ddr ()
   (prepare-display-window
     :name "delta Duration"
     :y-name "delta DR  [ms]"
     :y-scale 1
     :drawprop-fn 'ddr-fn
     ))

(defun draw-dr ()
   (prepare-display-window
     :name "Duration"
     :y-name "DR  [ms]"
     :y-scale 10
     :drawprop-fn 'dr-fn
     ))

(defun draw-bar-dr ()
   (prepare-display-window
     :name "Bar duration"
     :y-name "Bar duration [ms]"
     :y-scale 10
     :drawprop-fn 'bar-dr-fn
     ))

(defun draw-beat-dr ()
   (mark-beat)
   (prepare-display-window
     :name "Beat duration"
     :y-name "Beat duration [ms]"
     :y-scale 10
     :drawprop-fn 'beat-dr-fn
     ))

(defun draw-beat-dr% ()
   (mark-beat)
   (prepare-display-window
     :name "Beat duration (%)"
     :y-name "Beat duration [%]"
     :y-scale 0.25
     :drawprop-fn 'beat-dr%-fn
     ))

(defun draw-ddr% ()
   (prepare-display-window
     :name "delta Duration (%)"
     :y-name "delta DR [%]"
     :y-scale 0.25
     :drawprop-fn 'ddr%-fn
     ))

(defun draw-tempo ()
   (prepare-display-window
     :name "Tempo"
     :y-name "delta Tempo [%]"
     :y-scale 2
     :drawprop-fn 'tempo-fn
     ))

(defun draw-time-scoretime ()
   (prepare-display-window
     :name "Time - Scoretime"
     :y-name "Time - Scoretime [ms]"
     :y-scale 2
     :drawprop-fn 'time-scoretime-fn
     ))

(defun draw-droff ()
   (prepare-display-window
     :name "Offtime duration"
     :y-name "DRO [ms]"
     :y-scale 1
     :drawprop-fn 'droff-fn
     ))

(defun draw-dro%IOI ()
   (prepare-display-window
     :name "Offtime duration as % of IOI"
     :y-name "DRO [% of IOI]"
     :y-scale 1
     :drawprop-fn 'dro%IOI-fn
     ))

(defun draw-f0-mean ()
   (prepare-display-window
     :name "f0 mean"
     :y-name "f0 from C4, mean of 5  [tonnr]"
     :y-scale 1
     :drawprop-fn 'f0-mean-fn
     ))

(defun draw-dcent ()
   (prepare-display-window
     :name "delta Cent"
     :y-name "DC [cent]"
     :y-scale 1
     :drawprop-fn 'dcent-fn
     ))

(defun draw-va ()
   (prepare-display-window
     :name "Vibrato amp"
     :y-name "VA [cent]"
     :y-scale 0.25
     :drawprop-fn 'va-fn
     ))

(defun draw-vf ()
   (prepare-display-window
     :name "Vibrato freq"
     :y-name "VF [Hz]"
     :y-scale 0.1
     :drawprop-fn 'vf-fn
     ))

(defun draw-envelope ()
   (prepare-display-window
     :name "Envelope amplitude"
     :y-name "Envelope amplitude  [dB]"
     :y-scale 0.5
     :drawprop-fn 'env-fn
     ))

(defun draw-volume ()
   (prepare-display-window
     :name "Volume"
     :y-name "Volume  [dB]"
     :y-scale 0.5
     :drawprop-fn 'vol-fn
     ))

(defun draw-mi ()
   (prepare-display-window
     :name "mi"
     :y-name " mi    "
     :y-scale 1
     :drawprop-fn 'mi-fn
     ))

(defun draw-f1 ()
   (prepare-display-window
     :name "F1"
     :y-name "F1  [Hz]"
     :y-scale 10
     :drawprop-fn 'dr-f1
    ))
(defun draw-f2 ()
   (prepare-display-window
     :name "F2"
     :y-name "F2  [Hz]"
     :y-scale 10
     :drawprop-fn 'dr-f2
    ))
(defun draw-f3 ()
   (prepare-display-window
     :name "F2"
     :y-name "F2  [Hz]"
     :y-scale 10
     :drawprop-fn 'dr-f3
    ))
(defun draw-f4 ()
   (prepare-display-window
     :name "F2"
     :y-name "F2  [Hz]"
     :y-scale 10
     :drawprop-fn 'dr-f4
    ))


;;; ===================================
;;;    D R A W    T H E    G R A P H
;;; ===================================

;if propfn return nil there will not be any line on that note      *******?
;if propfn gives a value less than minimum it will be set to max-graf (min)

;; -------------
;;   DRAW-PROP
;; -------------
;;
(defun draw-prop (&key stream)
;;;  (with-focused-view stream      it's supposed to define a default stream/view/pane/window
    ;(assert (slot-boundp stream 'drawprop-fn) view "undefined drawprop-fn!")
    (gp-draw-prop :stream stream) )

(defun gp-draw-prop (&key stream)
   (let ((bar-nr 0)(xcur 4)(lastx 4)
         (lasty (yzero stream))
         (yzero (yzero stream))
         (propfn (drawprop-fn stream))
         (start-bar (start-bar stream))
         (bar-in-score? nil) )
      (gp-select-text-font :stream stream)
      (draw-y-axis :stream stream)
      (setq xcur (xstart stream))
      (setq lastx (xstart stream))
      
      (each-segment
        (when (this 'bar) (incf bar-nr) (setq bar-in-score? t))
       (if (or (>= bar-nr start-bar)
               (not bar-in-score?) )
           (then
             (let ((dx (round (/ (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)) (x-scale stream)))))
                (gp-move-to xcur (+ yzero 2) :stream stream)  ;x-axis
                (gp-line 0 -4 :stream stream)                 ;draw the vertical bar
                (gp-move 2 0 :stream stream)
                (if (this 'bar)                  ;draw the bar number
                   (gp-write-string (prin1-to-string (this 'bar)) :stream stream) )
                (gp-move-to xcur yzero :stream stream)        ;draw the x-axis part dotted
                (gp-line-to (+ xcur dx) yzero :stream stream)
                (cond
                      ((and (funcall propfn)             ;time envelope?
                            (typep (funcall propfn) 'shape))
                       ;(print "time envelope")
                       (gp-move-to xcur lasty :stream stream)
                       (setq lasty (draw-env-one-note (funcall propfn) xcur yzero dx :stream stream))
                       (incf xcur dx) )
                      ((square-plot? stream)                   ;square plot?
                       ;(print "square plot")
                       (gp-move-to xcur lasty :stream stream)
                       (if (not (call-fn :stream stream))
                         (incf xcur dx)
                         (progn
                           (gp-line-to  xcur (setq lasty (call-fn :stream stream)) :stream stream)   ;first vertical part
                           (incf xcur dx)
                           (gp-line-to xcur lasty :stream stream) )))         ;horizontal part
                      (t                                     ;else
                        (gp-move-to lastx lasty :stream stream)
                        (if (call-fn :stream stream)
                           (then
                             (gp-line-to xcur (setq lasty (call-fn :stream stream)) :stream stream)
                             (gp-paint-oval (- xcur 2)(- lasty 1)(+ xcur 2)(+ lasty 3) :stream stream) )
                           (gp-line-to xcur (+ 10 lasty) :stream stream) )   ; (+ 10 lasty)=lasty
                        (setq lastx xcur)
                        (incf xcur dx) ))
                (if (> xcur (xmax stream))
                   (return nil) )  ; exit-voice...
                ))
           )))
   (if (get-dm-var 'print-drm-p) (draw-prop-drm :stream stream))
   (if (notes? stream) (drawprop-notes :stream stream)) 
   )
;;1102/af new version with one track display
(defun gp-draw-prop (&key stream)
  (let ((bar-nr 0)(xcur 4)(lastx 4)
        (lasty (yzero stream))
        (yzero (yzero stream))
        (propfn (drawprop-fn stream))
        (start-bar (start-bar stream))
        (start-track (start-track stream))
        (bar-in-score? nil) )
    (gp-select-text-font :stream stream)
    (draw-y-axis :stream stream)
    (setq xcur (xstart stream))
    (setq lastx (xstart stream))
    
    (each-track
     (when (= (1- start-track) *i*)
       ;(print-ll "start-track " start-track "  *i* " *i*)
       
       (each-segment
        (when (this 'bar) (incf bar-nr) (setq bar-in-score? t))
        (if (or (>= bar-nr start-bar)
                (not bar-in-score?) )
            (then
             (let ((dx (round (/ (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)) (x-scale stream)))))
               (gp-move-to xcur (+ yzero 2) :stream stream)  ;x-axis
               (gp-line 0 -4 :stream stream)                 ;draw the vertical bar
               (gp-move 2 0 :stream stream)
               (if (this 'bar)                  ;draw the bar number
                   (gp-write-string (prin1-to-string (this 'bar)) :stream stream) )
               (gp-move-to xcur yzero :stream stream)        ;draw the x-axis part dotted
               (gp-line-to (+ xcur dx) yzero :stream stream)
               (cond
                ((and (funcall propfn)             ;time envelope?
                      (typep (funcall propfn) 'shape))
                 ;(print "time envelope")
                 (gp-move-to xcur lasty :stream stream)
                 (setq lasty (draw-env-one-note (funcall propfn) xcur yzero dx :stream stream))
                 (incf xcur dx) )
                ((square-plot? stream)                   ;square plot?
                 ;(print "square plot")
                 (gp-move-to xcur lasty :stream stream)
                 (if (not (call-fn :stream stream))
                     (incf xcur dx)
                   (progn
                     (gp-line-to  xcur (setq lasty (call-fn :stream stream)) :stream stream)   ;first vertical part
                     (incf xcur dx)
                     (gp-line-to xcur lasty :stream stream) )))         ;horizontal part
                (t                                     ;else
                 (gp-move-to lastx lasty :stream stream)
                 (if (call-fn :stream stream)
                     (then
                      (gp-line-to xcur (setq lasty (call-fn :stream stream)) :stream stream)
                      (gp-paint-oval (- xcur 2)(- lasty 1)(+ xcur 2)(+ lasty 3) :stream stream) )
                   (gp-line-to xcur (+ 10 lasty) :stream stream) )   ; (+ 10 lasty)=lasty
                 (setq lastx xcur)
                 (incf xcur dx) ))
               (if (> xcur (xmax stream))
                   (return nil) )  ; exit-voice...
               ))
          )) ))
    )
  (if (get-dm-var 'print-drm-p) (draw-prop-drm :stream stream))
  (if (notes? stream) (drawprop-notes :stream stream)) 
  )


(defun ddrm%-fn ()
  (if (this :drm)
    (* (/ (- (this :drm) (this 'ndr)) (this 'ndr)) 100.)
    nil ))


;draw a second graph above as ddr% but with the prop :drm
(defun draw-prop-drm (&key stream)
  (let ((bar-nr 0)(xcur 4)(lastx 4)
        (lasty (yzero stream))
        (yzero (yzero stream))
        (propfn 'ddrm%-fn)
        (start-bar (start-bar stream)) )
    (setq xcur (xstart stream))
    (setq lastx (xstart stream))

    (each-note
     (if (this 'bar) (incf bar-nr))
     (if (>= bar-nr start-bar)
      (then
       (let ((dx (round (/ (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)) (x-scale stream)))))
        (cond
         ((and (funcall propfn)             ;time envelope?
               (listp (funcall propfn)))
          (draw-env-one-note (funcall propfn) xcur yzero dx :stream stream)
          (incf xcur dx) )
         ((square-plot? stream)                   ;square plot?
          (gp-move-to xcur lasty :stream stream)
          (if (not (call-fn-drm :stream stream))
               (incf xcur dx)
               (progn
                 (gp-line-to xcur (setq lasty (call-fn-drm :stream stream)) :stream stream)   ;first vertical part
                 (incf xcur dx)
                 (gp-line-to xcur lasty :stream stream) )))         ;horisontal part
         (t                                     ;else
          (gp-move-to lastx lasty :stream stream)
          (if (call-fn-drm :stream stream)
            (then       
              ; check the original: with-fore-color *blue-color* &&  (#_PenPat *gray-pattern*)
                (gp-line-to xcur (setq lasty (call-fn-drm :stream stream)))
              ;(#_PenNormal)
              ;(gp-paint-oval (- xcur 2)(- lasty 1)(+ xcur 2)(+ lasty 3))
              )
            (gp-line-to xcur lasty :stream stream) )
          (setq lastx xcur)
          (incf xcur dx) ))
        (if (> xcur (xmax stream))
          (exit-voice) )
        )))))
  )

;;draw a time-level list of parameters for one note
;;returns the last y value
;;; (defun draw-env-one-note (tl-list x y dx &key stream)
;;;   (let ((y-scale (y-scale stream))
;;;         (time)(level))
;;;     (while tl-list
;;;       (setq time (pop tl-list)) 
;;;       (setq level (pop tl-list))
;;;       (gp-line-to (round (+ x (* dx (/ time (if (xaxis-ndr? stream)(this 'ndr)(this 'dr))))))
;;;                (round (- y (/ level y-scale))) :stream stream) )
;;;     (when (< time (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)))
;;;       (gp-line-to (round (+ x dx))
;;;                (round (- y (/ level y-scale))) :stream stream) )
;;;     (round (- y (/ level y-scale)))
;;;     ))
(defun draw-env-one-note (shape x y dx &key stream)
  (let ((y-scale (y-scale stream))
        level time
        (npix 5) ;update every npix pixel
        (dr (if (xaxis-ndr? stream)(this 'ndr)(this 'dr))) )
     ;(print dx)
    (loop for xtime from 0 to (get-x-max shape) by (* npix (/ dr dx)) do
      (setq time xtime)
      (setq level (get-value-at shape time))
      (gp-line-to (round (+ x (* dx (/ time dr))))
               (round (- y (/ level y-scale))) :stream stream) )
;;;     (when (< time dr)
;;;       (gp-line-to (round (+ x dx))
;;;                (round (- y (/ level y-scale))) :stream stream) )
    (round (- y (/ level y-scale)))
    ))

;fast drawing for linear shapes
;otherwise sample the envelope at npix rate
(defun draw-env-one-note (shape x y dx &key stream)
  (let ((y-scale (y-scale stream))
        level time
        (npix 5) ;update every npix pixel
        (dr (if (xaxis-ndr? stream)(this 'ndr)(this 'dr))) )
    ;(print dx)
    (cond
     ((equal 'LINEAR-BP-SHAPE (type-of shape))
      (loop for i from 0 to (1- (GET-N-OF-BP shape)) do
            (setq time (get-x-value shape i))
            (setq level (get-y-value shape i))
            (gp-line-to (round (+ x (* dx (/ time dr))))
                        (round (- y (/ level y-scale))) :stream stream) ))
     (t
      (loop for xtime from 0 to (get-x-max shape) by (* npix (/ dr dx)) do
      (setq time xtime)
      (setq level (get-value-at shape time))
      (gp-line-to (round (+ x (* dx (/ time dr))))
               (round (- y (/ level y-scale))) :stream stream) )) )
       
    (round (- y (/ level y-scale)))
    ))



;used by draw-prop
;returns the new y value in mac-local units
;return nil when nil from propfn or if propfn returns a list
;without lower limit
(defun call-fn (&key stream)
 ;(print "call fn")
 (let ((val (funcall (drawprop-fn stream))))
  (if (and val (not (listp val)))
       (let ((y (- (yzero stream)
                   (round (/ val (y-scale stream))))))
             y )
       nil )))

(defun call-fn-drm (&key stream)
 (let ((val (ddrm%-fn)))
  (if (and val (not (listp val)))
       (let ((y (- (yzero stream)
                   (round (/ val (y-scale stream))))))
             y )
       nil )))

;; ---------------
;;   DRAW-Y-AXIS
;; ---------------

(defun draw-y-axis (&key stream)
   (let ((yrel 0)
         (xstart (xstart stream))
         (yzero (yzero stream)))
      (gp-move-to xstart (yzero stream) :stream stream)
      (loop for y from (- yzero 20) downto (+ (ymin stream) 10) by 20 do  ;the positive part
        (incf yrel 20)
        (gp-line-to xstart y :stream stream)
        (gp-move -1 0 :stream stream)
        (gp-line 2 0 :stream stream)
        (let ((str (string-number (* yrel (y-scale stream)))))
           (gp-move (- (+ (* (length str) 6) 3)) 3 :stream stream)
           (gp-write-string str :stream stream) )
        (gp-move-to xstart y :stream stream) )
      (gp-write-string (concatenate 'string "  " (y-name stream)) :stream stream)
      
      (gp-move-to xstart yzero :stream stream)           ;the negative part
      (setq yrel 0)
      (loop for y from (+ yzero 20) to (ymax-graf stream) by 20 do
        (incf yrel -20)
        (gp-line-to xstart y :stream stream)
        (gp-move -1 0 :stream stream)
        (gp-line 2 0 :stream stream)
        (let ((str (string-number (* yrel (y-scale stream)))))
           (gp-move (- (+ (* (length str) 6) 3)) 3 :stream stream)
           (gp-write-string str :stream stream) )
        (gp-move-to xstart y :stream stream) )
      ))
    
(defun string-number (nr)
  (cond ((integerp nr)
         (prin1-to-string nr) )
        ((zerop (- (truncate nr) nr))
         (prin1-to-string (truncate nr)) )
        (t "") ))


;;-------------- draw the notes ---------------------------------


;to be used with parameter display
(defun drawprop-notes (&key stream)
   (let ((yg (round (+ (/ (- (ymax-controls stream)      ;current y pos of the note G 
                             (ymax-notes stream)) 2.0)
                       (ymax-notes stream))))
         (xcur (+ (xmin stream) 10))                ;current x position
         ;(ymin (ymax-notes stream))
         ;(ymax (ymax-controls stream))
         (bar-nr 0) )
      (gp-select-music-font :stream stream)
      (draw-music-lines xcur (xmax stream) yg :stream stream)
      (draw-bar xcur yg :stream stream)
      (draw-g xcur yg :stream stream)
      (setq xcur (xstart stream))
      (each-segment 
        (if (this 'bar) (incf bar-nr))
        (if (>= bar-nr (start-bar stream))
           (then
             (let ((dx (round (/ (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)) (x-scale stream)))))
                (if (this 'rest)
                   (draw-rest xcur yg (note-to-notevalue (this 'n)) (this 'dot) nil :stream stream)
                   (draw-note xcur 
                     (if (get-dm-var 'to-printer?) (- yg 1) yg) (this 'n) (this 'dot) nil :stream stream))
                (incf xcur dx)
                (cond ((and (not (last?)) (next 'bar))
                       (draw-bar (- xcur 3) yg :stream stream) ))
                (if (>= xcur (xmax stream))
                   (return nil) )
               ))))))

(defun drawprop-notes (&key stream)
  (let ((yg (round (+ (/ (- (ymax-controls stream)      ;current y pos of the note G 
                            (ymax-notes stream)) 2.0)
                      (ymax-notes stream))))
        (xcur (+ (xmin stream) 10))                ;current x position
        ;(ymin (ymax-notes stream))
        ;(ymax (ymax-controls stream))
        (bar-nr 0)
        (bar-in-score? nil) )
    (gp-select-music-font :stream stream)
    (draw-music-lines xcur (xmax stream) yg :stream stream)
    (draw-bar xcur yg :stream stream)
    (draw-g xcur yg :stream stream)
    (setq xcur (xstart stream))
    (each-segment 
     (when (this 'bar) (incf bar-nr) (setq bar-in-score? t))
     (if (or (>= bar-nr (start-bar stream))
             (not bar-in-score?) )
         (then
          (let ((dx (round (/ (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)) (x-scale stream)))))
            
            ;;;                (if (this 'rest)
            ;;;                   (draw-rest xcur yg (note-to-notevalue (this 'n)) (this 'dot) nil :stream stream)
            ;;;                   (draw-note xcur 
            ;;;                              (if (get-dm-var 'to-printer?) (- yg 1) yg) (this 'n) (this 'dot) nil :stream stream))
            
            (cond
             ((and (this 'rest) (this 'n))
              (draw-rest (round xcur) yg (note-to-notevalue (this 'n)) (this 'dot)
                         nil :tuple (or (this 'tuple) (this 't)) :stream stream ))
             ((this 'n)
              (draw-note (round xcur) yg (this 'n) (this 'dot)
                         nil :tuple (or (this 'tuple) (this 't)) :stream stream) )
             ((this 'f0)
              (draw-roll-bar (round xcur) dx yg (this 'f0)
                             nil :stream stream) )
             (t ;rest and no note
              (draw-rest-roll-bar (round xcur) dx yg
                                  nil :stream stream) ))
            
            (incf xcur dx)
            (cond ((and (not (last?)) (next 'bar))
                   (draw-bar (- xcur 3) yg :stream stream) ))
            (if (>= xcur (xmax stream))
                (return nil) )
            ))))))

;;1102/af new version with one track display
(defun drawprop-notes (&key stream)
  (let ((yg (round (+ (/ (- (ymax-controls stream)      ;current y pos of the note G 
                            (ymax-notes stream)) 2.0)
                      (ymax-notes stream))))
        (xcur (+ (xmin stream) 10))                ;current x position
        ;(ymin (ymax-notes stream))
        ;(ymax (ymax-controls stream))
        (bar-nr 0)
        (bar-in-score? nil)
        (start-track (start-track stream)) )
    (gp-select-music-font :stream stream)
    (draw-music-lines xcur (xmax stream) yg :stream stream)
    (draw-bar xcur yg :stream stream)
    (draw-g xcur yg :stream stream)
    (setq xcur (xstart stream))
    
    (each-track
     (when (= (1- start-track) *i*)
       ;(print-ll "start-track " start-track "  *i* " *i*)
       
       (each-segment 
        (when (this 'bar) (incf bar-nr) (setq bar-in-score? t))
        (if (or (>= bar-nr (start-bar stream))
                (not bar-in-score?) )
            (then
             (let ((dx (round (/ (if (xaxis-ndr? stream)(this 'ndr)(this 'dr)) (x-scale stream)))))
               
               ;;;                (if (this 'rest)
               ;;;                   (draw-rest xcur yg (note-to-notevalue (this 'n)) (this 'dot) nil :stream stream)
               ;;;                   (draw-note xcur 
               ;;;                              (if (get-dm-var 'to-printer?) (- yg 1) yg) (this 'n) (this 'dot) nil :stream stream))
               
               (cond
                ((and (this 'rest) (this 'n))
                 (draw-rest (round xcur) yg (note-to-notevalue (this 'n)) (this 'dot)
                            nil :tuple (or (this 'tuple) (this 't)) :stream stream ))
                ((this 'n)
                 (draw-note (round xcur) yg (this 'n) (this 'dot)
                            nil :tuple (or (this 'tuple) (this 't)) :stream stream) )
                ((this 'f0)
                 (draw-roll-bar (round xcur) dx yg (this 'f0)
                                nil :stream stream) )
                (t ;rest and no note
                 (draw-rest-roll-bar (round xcur) dx yg
                                     nil :stream stream) ))
               
               (incf xcur dx)
               (cond ((and (not (last?)) (next 'bar))
                      (draw-bar (- xcur 3) yg :stream stream) ))
               (if (>= xcur (xmax stream))
                   (return nil) )
               ))))))))


;;---------- save to PICT file ------------------

;;; (defmethod window-save-as ((w display-window))
;;;   (let ((fpath (choose-new-file-dialog 
;;;                 :directory (merge-pathnames ".pict" (or (get-filename) "temp")))))
;;;     (when fpath
;;;       (with-cursor *watch-cursor*
;;;         (with-focused-view w
;;;           (with-pict-output-file (fpath w #@(0 0) (view-size w))
;;;             (draw-prop (display-scrollee w)) ))
;;;         (set-mac-file-type fpath "PICT")
;;;         (set-mac-file-creator fpath "SPNT")
;;;         ))))

;;(display-pict-file "hd:ekor.pict" 0.5)


;;---- parameter access functions ----------------

;;; (defun da0-fn ()
;;;   (if (this 'da0)
;;;     (* 0.1 (this 'da0))
;;;     nil))
;;; (defun da0-fn ()
;;;  (* 0.1 (or (this 'da0) 0)))

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
  (if (and
       (this :beat)
       (i?next *i* :beat) )
    (drsum *i* (i?next *i* :beat))
    0))

(defun beat-dr%-fn ()
  (if (and
       (this :beat)
       (i?next *i* :beat) )
     (* (/ (- (drsum *i* (i?next *i* :beat))
              (ndrsum *i* (i?next *i* :beat)))
           (ndrsum *i* (i?next *i* :beat))) 100.)
    0))

(defun ddr%-fn ()
 (* (/ (- (this 'dr) (this 'ndr)) (this 'ndr)) 100.))

(defun tempo-fn ()
 (* (/ (this 'ndr)(this 'dr)) 100.))

(defun time-scoretime-fn ()
  (- (drsum 0 *i*) (ndrsum 0 *i*)) )

(defun droff-fn ()
 (if (this 'dro)(this 'dro) 0))

;; dro%IOI = dro/IOI
;;;(defun dro%IOI-fn ()
;;; (if (this 'dro)(*(/(this 'dro)(this 'ndr)) 100) 0))
(defun dro%IOI-fn ()
 (if (this 'dro)(*(/(this 'dro)(this 'dr)) 100) 0))

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

(defun dr-f1 () (this 'f1))
(defun dr-f2 () (this 'f2))
(defun dr-f3 () (this 'f3))
(defun dr-f4 () (this 'f4))


;;eof

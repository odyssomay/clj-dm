
;; _________________ basic music drawing functions ______________________
;; returns new proportional x positions

;; Anders Friberg  October 1987.
;; 2000-10-19/af added 

(in-package :dm)

(require :quickdraw)

;(defvar *music-font* 242) ;sonata
;(defvar *music-font-size* 18)
;(defvar *text-font* 21)
;(defvar *text-font-size* 9)

;(defun select-text-font (view)
;   (set-view-font view '("Times" 9)))
;(defun select-music-font (view)
;   (set-view-font view '("Sonata" 18)))

(defun gp-select-text-font ()
  (multiple-value-bind (a b c d) (font-codes '("Times" 9))
    (set-grafport-font-codes a b c d)))
 
(defun gp-select-music-font ()
  (multiple-value-bind (a b c d) (font-codes '("Sonata" 18))
    (set-grafport-font-codes a b c d)))

(defun count-bars ()
 (let ((bars 0))
  (untilexit voice
  (each-note
    (if (this 'bar) (incf bars))
    (if (last?) (return-from voice)) ))
  bars ))  



;; ------------
;;  RESTS
;; ------------
;;


(defun draw-one-rest (x ye note dot)
  (let ((start-x x))
   (gp-move-to (+ 2 x) (y-pos ye (posnr "A" 4)))
   (case note            
     (1 (gp-move-to (+ 10 x) (y-pos ye (posnr "C" 5)))
      (#_DrawChar (character 238))(incf x 45))
     (2 (gp-move-to (+ 7 x) (- (y-pos ye (posnr "B" 4)) 1))
      (#_DrawChar (character 238)) (incf x 35))
     (4 (#_DrawChar (character 206)) (incf x 25))
     (8 (#_DrawChar (character 228)) (incf x 15))
     (16 (#_DrawChar (character 197)) (incf x 10))
     (32 (#_DrawChar (character 168)) (incf x 10)) )
   (cond (dot (gp-move 3 0) (#_DrawChar (character 46))))
   ;(if prop-val-list (print-val-list (+ 2 start-x) (+ 4 ye) prop-val-list))
    x ))

;;added handling of strange midi notes
(defun draw-one-rest (x ye note dot)
   (gp-move-to (+ 2 x) (y-pos ye (posnr "A" 4)))
   (case note            
     (1 (gp-move-to (+ 10 x) (y-pos ye (posnr "C" 5)))
      (#_DrawChar (character 238))(incf x 45))
     (2 (gp-move-to (+ 7 x) (- (y-pos ye (posnr "B" 4)) 1))
      (#_DrawChar (character 238)) (incf x 35))
     (4 (#_DrawChar (character 206)) (incf x 25))
     (8 (#_DrawChar (character 228)) (incf x 15))
     (16 (#_DrawChar (character 197)) (incf x 10))
     (32 (#_DrawChar (character 168)) (incf x 10))
     (64 (#_DrawChar (character 244)) (incf x 10))
     (t (#_DrawChar (character 192)) (incf x 10)) )
   (cond (dot (gp-move 3 0) (#_DrawChar (character 46))))
   ;(if prop-val-list (print-val-list (+ 2 start-x) (+ 4 ye) prop-val-list))
    x )

(defun draw-rest (x ye note dot prop-val-list)
      (if (listp note)
         (let ((i 0) (dx 0))
            (untilexit loop
              (if (>= i (length note)) (return-from loop))
              (cond 
                    ((and (< i (1- (length note)))           ;print dotted
                          (= 2 (/ (nth i note) (nth (1+ i) note))) )
                     (draw-one-rest (+ x dx) ye 
                       (denominator (nth i note)) t)
                     (incf i)
                     (incf dx 10) )
                    ((and (= 3 (numerator (nth i note)))           ;print dotted
                          (member (denominator (nth i note)) '(1 2 4 8 16 32 64)) )
                     (draw-one-rest (+ x dx) ye 
                       (/ (denominator (nth i note)) 2) t)
                     (incf dx 10) )
                    ((and (= 1 (numerator (nth i note)))           ;triplets
                          (member (denominator (nth i note)) '(3 6 12 24 48 96 192)) )
                     (draw-one-rest (+ x dx) ye 
                       (/ (* (denominator (nth i note)) 2) 3) nil)
                     (incf dx 10) )
                    (t 
                      (dotimes (i (numerator (nth i note)))
                         (draw-one-rest  (+ x dx) ye 
                           (denominator (nth i note)) nil)
                         (incf dx 15) ))
                    )
              (incf i)
              ))
         (draw-one-rest x ye note dot)
         )
      (if prop-val-list
         (print-val-list x (+ ye 4) prop-val-list)
         )
      )

(defun draw-rest (x ye note dot prop-val-list)
      (if (listp note)
         (let ((i 0) (dx 0))
            (untilexit loop
              (if (>= i (length note)) (return-from loop))
              (cond 
                    ((and (< i (1- (length note)))           ;print dotted
                          (= 2 (/ (nth i note) (nth (1+ i) note))) )
                     (draw-one-rest (+ x dx) ye 
                       (denominator (nth i note)) t)
                     (incf i)
                     (incf dx 10) )
                    ((and (= 3 (numerator (nth i note)))           ;print dotted
                          (member (denominator (nth i note)) '(1 2 4 8 16 32 64)) )
                     (draw-one-rest (+ x dx) ye 
                       (/ (denominator (nth i note)) 2) t)
                     (incf dx 10) )
                    ((and (= 1 (numerator (nth i note)))           ;triplets
                          (member (denominator (nth i note)) '(3 6 12 24 48 96 192)) )
                     (draw-one-rest (+ x dx) ye 
                       (/ (* (denominator (nth i note)) 2) 3) nil)
                     (incf dx 10) )
                    (t 
                      ;(dotimes (j (numerator (nth i note)))
                         (draw-one-rest  (+ x dx) ye 
                           (denominator (nth i note)) nil)
                         (incf dx 15) 
                        ; )
                      )
                    )
              (incf i)
              ))
         (draw-one-rest x ye note dot)
         )
      (if prop-val-list
         (print-val-list x (+ ye 4) prop-val-list)
         )
      )

;(defun foo () (draw-note 50 50 "C" 4 8 t))

;; ------------
;;  DRAW NOTES
;; ------------
;;
(defun draw-one-note (x ye tone oct note dot &key direction)
  (let* ((posnr (posnr tone oct))
         (y (y-pos ye posnr))
         (newx x) )
    (cond ((flat-tone? tone)
           (gp-move-to (+ x -5) y)
           (#_DrawChar (character 98)) )
          ((sharp-tone? tone)
           (gp-move-to (+ x -6) y)
           (#_DrawChar (character 35)) ))
    (draw-helplines x ye posnr)
    (gp-move-to x y)
    (if (not direction)
      (if (< posnr 7)
        (setq direction :up)
        (setq direction :down) ))
    (cond
     ((equal direction :up)
      (gp-move-to x (1+ y))
      (case note             ;stem up
        (1 (#_DrawChar (character 119)) (incf newx 60))
        (2 (#_DrawChar (character 104)) (incf newx 45))
        (4 (#_DrawChar (character 113)) (incf newx 25))
        (8 (#_DrawChar (character 101)) (incf newx 15))
        (16 (#_DrawChar (character 120)) (incf newx 12))
        (32 (#_DrawChar (character 114)) (incf newx 12)) ))
     ((equal direction :down)
      (gp-move-to x y)
      (case note             ;stem dowm
        (1 (#_DrawChar (character 119)) (incf newx 60))
        (2 (#_DrawChar (character 72)) (incf newx 45))
        (4 (#_DrawChar (character 81)) (incf newx 25))
        (8 (#_DrawChar (character 69)) (incf newx 15))
        (16 (#_DrawChar (character 88)) (incf newx 12))
        (32 (#_DrawChar (character 82)) (incf newx 12)) )))
    (cond (dot
           (gp-move-to (+ 8 x) y)
           (#_DrawChar (character 46)) ))
    newx ))

(defun draw-one-note (x ye tone oct note dot &key direction)
  (let* ((posnr (posnr tone oct))
         (y (y-pos ye posnr))
         (newx x) )
    (cond ((flat-tone? tone)
           (gp-move-to (+ x -5) y)
           (#_DrawChar (character 98)) )
          ((sharp-tone? tone)
           (gp-move-to (+ x -6) y)
           (#_DrawChar (character 35)) ))
    (draw-helplines x ye posnr)
    (gp-move-to x y)
    (if (not direction)
      (if (< posnr 7)
        (setq direction :up)
        (setq direction :down) ))
    (cond
     ((equal direction :up)
      (gp-move-to x (1+ y))
      (case note             ;stem up
        (1 (#_DrawChar (character 119)) (incf newx 60))
        (2 (#_DrawChar (character 104)) (incf newx 45))
        (4 (#_DrawChar (character 113)) (incf newx 25))
        (8 (#_DrawChar (character 101)) (incf newx 15))
        (16 (#_DrawChar (character 120)) (incf newx 12))
        (32 (#_DrawChar (character 114)) (incf newx 12))
        (64 (#_DrawChar (character 198)) (incf newx 12))
        (t (#_DrawChar (character 192)) (incf newx 12)) ))
     ((equal direction :down)
      (gp-move-to x y)
      (case note             ;stem dowm
        (1 (#_DrawChar (character 119)) (incf newx 60))
        (2 (#_DrawChar (character 72)) (incf newx 45))
        (4 (#_DrawChar (character 81)) (incf newx 25))
        (8 (#_DrawChar (character 69)) (incf newx 15))
        (16 (#_DrawChar (character 88)) (incf newx 12))
        (32 (#_DrawChar (character 82)) (incf newx 12))
        (64 (#_DrawChar (character 239)) (incf newx 12))
        (t (#_DrawChar (character 192)) (incf newx 12)) )))
    (cond (dot
           (gp-move-to (+ 8 x) y)
           (#_DrawChar (character 46)) ))
    newx ))

(defun draw-one-note-or-chord (x ye n dot)
   (let ((tone (car n))
         (note (cdr n))
         (newx)
         (direction nil) )
      (if (listp tone)
         ;CHORD
         (let ((meanposnr (round (/ (+ (posnr (toneoctave-to-tone (first tone))
                                         (toneoctave-to-octave (first tone)))
                                       (posnr (toneoctave-to-tone (car (last tone)))
                                         (toneoctave-to-octave (car (last tone)))) )
                                    2.0))))
            (cond 
                  ((< meanposnr 7)
                   (setq direction :up) )
                  (t
                    (setq direction :down)
                    (setq tone (reverse tone)) ))
            (setq newx (draw-one-note x ye (toneoctave-to-tone (car tone))
                         (toneoctave-to-octave (car tone)) note dot :direction direction) )
            (dolist (tone (cdr tone))
               (draw-one-note x ye (toneoctave-to-tone tone)
                 (toneoctave-to-octave tone) 
                 (if (< note 8) note 4)
                 dot :direction direction
                 )
               ))
         
         ;ONLY ONE NOTE
         (setq newx (draw-one-note x ye (toneoctave-to-tone tone)
                      (toneoctave-to-octave tone) note dot)
           ))
    newx ))

;including new notevalue format used by load-midifile /af
(defun draw-note (x ye n dot prop-val-list)
   (let ((tone (car n))
         (note (cdr n))
         )
      (if (listp note)
         (let ((i 0) (dx 0))
            (untilexit loop
              (if (>= i (length note)) (return-from loop))
              (cond 
                    ((and (< i (1- (length note)))           ;print dotted
                          (= 2 (/ (nth i note) (nth (1+ i) note))) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (denominator (nth i note))) t)
                     (incf i)
                     (incf dx 10) )
                    ((and (= 3 (numerator (nth i note)))           ;print dotted
                          (member (denominator (nth i note)) '(1 2 4 8 16 32 64)) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (/ (denominator (nth i note)) 2)) t)
                     (incf dx 10) )
                    ((and (= 1 (numerator (nth i note)))           ;triplets
                          (member (denominator (nth i note)) '(3 6 12 24 48 96 192)) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (/ (* (denominator (nth i note)) 2) 3)) nil)
                     (incf dx 10) )
                    (t
                      (dotimes (i (numerator (nth i note)))
                         (draw-one-note-or-chord (+ x dx) ye 
                           (cons tone (denominator (nth i note))) nil)
                         (incf dx 10) ))
                    )
              (incf i)
              ))
         (draw-one-note-or-chord x ye n dot)
         )
      (if prop-val-list
         (print-val-list x (+ ye 4) prop-val-list)
;;;          (progn
;;;            (if (listp tone) (setq tone (car tone)))
;;;            (print-val-list x (y-pos ye (posnr (toneoctave-to-tone tone)
;;;                                          (toneoctave-to-octave tone) ))
;;;              prop-val-list)
;;;            )
         )
      ))
;;new attempt to fix midi unquantized notes
(defun draw-note (x ye n dot prop-val-list)
   (let ((tone (car n))
         (note (cdr n))
         )
      (if (listp note)
         (let ((i 0) (dx 0))
            (untilexit loop
              (if (>= i (length note)) (return-from loop))
              (cond 
                    ((and (< i (1- (length note)))           ;print dotted
                          (= 2 (/ (nth i note) (nth (1+ i) note))) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (denominator (nth i note))) t)
                     (incf i)
                     (incf dx 10) )
                    ((and (= 3 (numerator (nth i note)))           ;print dotted
                          (member (denominator (nth i note)) '(1 2 4 8 16 32 64)) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (/ (denominator (nth i note)) 2)) t)
                     (incf dx 10) )
                    ((and (= 1 (numerator (nth i note)))           ;triplets
                          (member (denominator (nth i note)) '(3 6 12 24 48 96 192)) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (/ (* (denominator (nth i note)) 2) 3)) nil)
                     (incf dx 10) )
                    (t
                      ;(dotimes (j (numerator (nth i note)))
                         (draw-one-note-or-chord (+ x dx) ye 
                           (cons tone (denominator (nth i note))) nil)
                         (incf dx 10) 
                         ;)
                      )
                    )
              (incf i)
              ))
         (draw-one-note-or-chord x ye n dot)
         )
      (if prop-val-list
         (print-val-list x (+ ye 4) prop-val-list)
;;;          (progn
;;;            (if (listp tone) (setq tone (car tone)))
;;;            (print-val-list x (y-pos ye (posnr (toneoctave-to-tone tone)
;;;                                          (toneoctave-to-octave tone) ))
;;;              prop-val-list)
;;;            )
         )
      ))

(defun print-val-list (x y list)
  ;(print list)
  (let ((dy 8)(y-start 2))
    (gp-select-text-font)
    (dolist (val list)
      (gp-move-to x (+ y (incf dy 10) y-start))
      ;(print val)
      (if val (gp-write-string (princ-to-string val))) )
    (gp-select-music-font) ))


(defun draw-helplines (x ye posnr)
 (cond ((> posnr 11)
        (gp-move-to (- x 1) (+ ye -20))
        ;(if (oddp posnr) (gp-move 0 2))
        (loop for i from 0 to (truncate (/ (- posnr 12) 2.)) do
          (gp-line 7 0)(gp-move -7 -4) ))
       ((< posnr 1)
        (gp-move-to (- x 1) (+ ye 4))
        (loop for i from 0 to (truncate (/ (- posnr) 2.)) do
          (gp-line 7 0)(gp-move -7 4) ))
))

;rel C4
(defun posnr (tone oct)
 (+
  (* (- oct 4) 7) 
  (case (char tone 0)
     (#\C 0)(#\D 1)(#\E 2)(#\F 3)(#\G 4)(#\A 5)(#\B 6) )))

;(defun sharp-tone? (tone)
;  (if (> (length tone) 1)
;      (char= #\# (char tone 1)) ))

;(defun flat-tone? (tone)
;  (if (> (length tone) 1)
;      (char= #\b (char tone 1)) ))

;rel ye
;;to-clip 2.1 for the sonata barlines
(defun y-pos (ye nr-rel-c)
    (+ ye 0 (- (round (* (if (get-dm-var 'toclip?) 2 2) (- nr-rel-c 2))))) )

(defun draw-bar (x ye)
   (gp-move-to x ye)
   (gp-line 0 -16)
   (incf x 2) 
   x )

(defun draw-music-lines (x1 x2 ye)
  (#_PenNormal)
   (loop for i from 0 downto -16 by 4 do
      (gp-move-to x1 (+ i ye))
      (gp-line-to x2 (+ i ye)) ))


(defun draw-g (x ye)
  (gp-move-to x ye)
  (#_DrawChar (character 38))
  (incf x 20)
  x )

;;----------- draw macros to quickdraw -------------------------------

#|
(defmacro gp-erase-rect (left top right bot)
  `(ccl::with-rectangle-arg (r ,left ,top ,right ,bot) (#_EraseRect r)))

(defmacro gp-move-to (h v)
  `(#_MoveTo :long (make-point ,h ,v)))

(defmacro gp-move (h v)
  `(#_Move :long (make-point ,h ,v)))

(defmacro gp-line-to (h v)
  `(#_LineTo :long (make-point ,h ,v)))

(defmacro gp-line (h v)
  `(#_Line :long (make-point ,h ,v)))

(defmacro gp-paint-oval ( left top right bot)
    `(ccl::with-rectangle-arg (r ,left ,top ,right ,bot) (#_PaintOval r)))

(defmacro gp-write-string (str)
  `(with-pstrs ((s ,str)) (#_DrawString s)))
|#


(defun gp-erase-rect (left top right bot)
  (ccl::with-rectangle-arg (r left top right bot) (#_EraseRect r)))

(defun gp-move-to (h v)
  (#_MoveTo :long (make-point h v)))

(defun gp-move (h v)
  (#_Move :long (make-point h v)))

(defun gp-line-to (h v)
  (#_LineTo :long (make-point h v)))

(defun gp-line (h v)
  (#_Line :long (make-point h v)))

(defun gp-paint-oval ( left top right bot)
    (ccl::with-rectangle-arg (r left top right bot) (#_PaintOval r)))

(defun gp-write-string (str)
  (with-pstrs ((s str)) (#_DrawString s)))

;-------------------

(provide :DrawBasic)

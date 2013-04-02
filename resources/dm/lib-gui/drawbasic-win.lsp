;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************
;;   basic music drawing functions
;; ****************************************

(in-package :dm)

;; returns new proportional x positions
;; symbol numbers can be derived from the character map accessory in the system
;;
;; 8710/Anders Friberg
;; 9702/Vittorio Colombo  ACL -MCL compatible  version
;; 2000-10-19/af added triplet printing, 64ths notes
;; 2001-04-17/af added printing for unknown notevalues

;; ************ treat the PD !!!!!!!!!!!

;; (defun gp-select-text-font ()
;; (defun gp-select-music-font ()
;; (defun count-bars ()
;; (defun draw-rest (x ye note dot prop-val-list)
;; (defun draw-one-note (x ye tone oct note dot &key direction)
;; (defun draw-note (x ye n dot prop-val-list)
;; (defun print-val-list (x y list)
;; (defun draw-helplines (x ye posnr)
;; (defun posnr (tone oct)
;; (defun sharp-tone? (tone)
;; (defun flat-tone? (tone)
;; (defun y-pos (ye nr-rel-c)
;; (defun draw-bar (x ye)
;; (defun draw-lines (x1 x2 ye)
;; (defun draw-g (x ye)
;; (defun gp-erase-rect (left top right bot)
;; (defun gp-move-to (h v)
;; (defun gp-move (h v)
;; (defun gp-line-to (h v)
;; (defun gp-line (h v)
;; (defun gp-paint-oval ( left top right bot)
;; (defun gp-write-string (str)


;; ----------------------
;;  GP-SELECT-TEXT-FONT
;; ----------------------
;;
;; i.t. it sets the default font for the graphic device
;;
(defun gp-select-text-font (&key stream)
   (progn
    (setf (font stream) (make-font :modern :arial 12))
    (setf (foreground-color stream) dark-blue)
    )
   )


;; -----------------------
;;  GP-SELECT-MUSIC-FONT
;; -----------------------
;;
;; i.t. it sets the default font for the graphic device
;;
#|
(defun gp-select-music-font (&key stream)
   (setf (font stream) (make-font :modern :sonata 64) ;64
))
(defun gp-select-music-font (&key stream)
   (progn
    (setf (font stream) (make-font :modern :anastasia 64)) ;64
    (setf (foreground-color stream) black)
    ))
(defun gp-select-music-font (&key stream)
   (progn
    (setf (font stream) (make-font-ex nil (get-dm-var 'music-font-face) 23)) ;64
    (setf (foreground-color stream) black)
    ))
|#
;; Anastasia
(defun gp-select-music-font (&key stream)
   (progn
    (setf (font stream) (make-font-ex nil (get-dm-var 'music-font-face) 19)) ;64
    (setf (foreground-color stream) black)
    ))

;; MusicalSymbols - not working ok - also looks less good
;;;(defun gp-select-music-font (&key stream)
;;;   (progn
;;;    (setf (font stream) (make-font-ex nil (get-dm-var 'music-font-face) 21)) ;64
;;;    (setf (foreground-color stream) black)
;;;     ))

;; ------------
;;  DRAW-REST
;; ------------
;;

(defun draw-one-rest (x ye note dot  &key (tuple nil) stream)
    (case note            
        (1 (gp-move-to (+ 10 x) (+ (y-pos ye (posnr "C" 5)) 2) :stream stream)
         (put-char 238 :stream stream)
         (if tuple (draw-tuple-number (+ x 10) (+ ye 0) tuple :stream stream))
         (incf x 45))
        (2 (gp-move-to (+ 7 x) (+ (y-pos ye (posnr "B" 4)) 2) :stream stream)
         (put-char 238 :stream stream)
         (if tuple (draw-tuple-number (+ x 8) (+ ye 0) tuple :stream stream))
         (incf x 35))
        (4 (gp-move-to (+ 2 x) (y-pos ye (posnr "A" 4)) :stream stream)
         (put-char 206 :stream stream)
         (if tuple (draw-tuple-number (+ x 2) (+ ye 9) tuple :stream stream))
         (incf x 25))
        (8 (gp-move-to (+ 2 x) (+ (y-pos ye (posnr "A" 4)) -4) :stream stream)
         (put-char 228 :stream stream)
         (if tuple (draw-tuple-number (+ x 3) (+ ye 4) tuple :stream stream))
         (incf x 15))
        (16 (gp-move-to (+ 2 x) (y-pos ye (posnr "A" 4)) :stream stream)
         (put-char 197 :stream stream)
         (if tuple (draw-tuple-number (+ x 3) (+ ye 9) tuple :stream stream))
         (incf x 10))
      (32 (gp-move-to (+ 2 x) (y-pos ye (posnr "A" 4)) :stream stream)
         (put-char 168 :stream stream)
         (if tuple (draw-tuple-number (+ x 3) (+ ye 9) tuple :stream stream))
         (incf x 10))
      (64 (gp-move-to (+ 2 x) (y-pos ye (posnr "F" 4)) :stream stream)
         (put-char 244 :stream stream)
         (if tuple (draw-tuple-number (+ x 3) (+ ye 9) tuple :stream stream))
         (incf x 10))
      (t (gp-move-to (+ 2 x) (y-pos ye (posnr "F" 4)) :stream stream)
         (put-char 192 :stream stream)
                (incf x 12)))
    (cond (dot (gp-move 2 -2 :stream stream)
               (put-char 46 :stream stream)))
    x )

;including new notevalue format used by load-midifile /af
(defun draw-rest (x ye note dot prop-val-list &key (tuple nil) stream)
      (if (listp note)
         (let ((i 0) (dx 0)) ;new list notation
            (untilexit loop
              (if (>= i (length note)) (return-from loop))
              (cond 
                    ((and (< i (1- (length note)))           ;print dotted
                          (= 2 (/ (nth i note) (nth (1+ i) note))) )
                     (draw-one-rest (+ x dx) ye 
                       (denominator (nth i note)) t :stream stream)
                     (incf i)
                     (incf dx 10) )
                    ((and (= 3 (numerator (nth i note)))           ;print dotted
                          (member (denominator (nth i note)) '(1 2 4 8 16 32 64)) )
                     (draw-one-rest (+ x dx) ye 
                       (/ (denominator (nth i note)) 2) t :stream stream)
                     (incf dx 10) )
                    ((and (= 1 (numerator (nth i note)))           ;triplets
                          (member (denominator (nth i note)) '(3 6 12 24 48 96 192)) )
                     (draw-one-rest (+ x dx) ye (/ (* (denominator (nth i note)) 2) 3) nil :tuple 3 :stream stream)
                     ;(draw-tuple-number (+ x 2) (+ ye 9) 3 :stream stream)
                     (incf dx 10) )
                    (t 
                      (dotimes (numerator (nth i note))
                         (draw-one-rest  (+ x dx) ye 
                           (denominator (nth i note)) nil :tuple tuple :stream stream)
                         (incf dx 15) ))
                    )
              (incf i)
              ))
         (draw-one-rest x ye note dot :tuple tuple :stream stream) ;old dot notation
         )
      (if prop-val-list
         (print-val-list x (+ ye 4) prop-val-list :stream stream)
         )
  )

;draws a number above the note
(defun draw-tuple-number (x y tuple &key stream)
  (gp-select-text-font :stream stream)
  (setf (foreground-color stream) black)
  (gp-move-to x y :stream stream)
  (gp-write-string (princ-to-string tuple) :stream stream)
  (gp-select-music-font :stream stream)
  )


;(defun foo () (draw-note 50 50 "C" 4 8 t))


;; ----------------
;;  DRAW-ONE-NOTE
;; ----------------
;;
#| ;;for printing all characters

(defun foo ()
  (each-note (set-this 'charnr *i*)))

(defun draw-one-note (x ye tone oct note dot &key (tuple nil) stream direction)
      (cond ((this 'charnr)
             (gp-move-to x (+ ye 30) :stream stream)
             (put-char (this 'charnr) :stream stream) )
            ))
|#

#|
(defun draw-one-note (x ye tone oct note dot &key stream direction)
  (let* ((posnr (posnr tone oct))
          (y (y-pos ye posnr))
          (newx x) )
      (cond ((flat-tone? tone)
             (gp-move-to (+ x -5) (+ y 2) :stream stream)
             (put-char 98 :stream stream) )
            ((sharp-tone? tone)
             (gp-move-to (+ x -6) (+ y 1) :stream stream)
             (put-char 35 :stream stream) ))
      (draw-helplines x ye posnr :stream stream)
      (gp-move-to x y :stream stream)
      (if (not direction)
         (if (< posnr 7)
            (setq direction :up)
            (setq direction :down) ))
      (cond
            ((equal direction :up)
             (gp-move-to x (1+ y) :stream stream)
             (case note             ;stem up
               (1 (put-char 119 :stream stream)
                (incf newx 60))
               (2 (put-char 104 :stream stream)
                (incf newx 45))
               (4 (put-char 113 :stream stream)
                (incf newx 25))
               (8 (put-char 101 :stream stream)
                (incf newx 15))
               (16 (put-char 120 :stream stream)
                (incf newx 12))
               (32 (put-char 114 :stream stream)
                (incf newx 12))
               (64 (put-char 198 :stream stream)
                (incf newx 12))))
            ((equal direction :down)
             (gp-move-to x (+ y 2) :stream stream)
             (case note             ;stem dowm
               (1 (put-char 119 :stream stream)
                (incf newx 60))
               (2 (put-char 72 :stream stream)
                (incf newx 45))
               (4 (put-char 81 :stream stream)
                (incf newx 25))
               (8 (put-char 69 :stream stream)
                (incf newx 15))
               (16 (put-char 88 :stream stream)
                (incf newx 12))
               (32 (put-char 82 :stream stream)
                (incf newx 12))
               (64 (put-char 239 :stream stream)
                (incf newx 12)))))
      (cond (dot
              (gp-move-to (+ 8 x) y :stream stream)
              (put-char 46 :stream stream) ))
    newx ))
|#
(defun draw-one-note (x ye tone oct note dot &key (tuple nil) stream direction)
  (let* ((posnr (posnr tone oct))
          (y (y-pos ye posnr))
          (newx x) )
      (cond ((flat-tone? tone)
             (gp-move-to (+ x -5) (+ y 2) :stream stream)
             (put-char 98 :stream stream) )
            ((sharp-tone? tone)
             (gp-move-to (+ x -6) (+ y 1) :stream stream)
             (put-char 35 :stream stream) ))
      (draw-helplines x ye posnr :stream stream)
      (gp-move-to x y :stream stream)
      (if (not direction)
         (if (< posnr 7)
            (setq direction :up)
            (setq direction :down) ))
      (cond
            ((equal direction :up)
             (gp-move-to x (1+ y) :stream stream)
             (case note             ;stem up
               (1 (put-char 119 :stream stream)
                (incf newx 60))
               (2 (put-char 104 :stream stream)
                (incf newx 45))
               (4 (put-char 113 :stream stream)
                (incf newx 25))
               (8 (put-char 101 :stream stream)
                (incf newx 15))
               (16 (put-char 120 :stream stream)
                (incf newx 12))
               (32 (put-char 114 :stream stream)
                (incf newx 12))
               (64 (put-char 198 :stream stream)
                (incf newx 12))
               (t (gp-move-to x (+ y 2) :stream stream)
                  (put-char 192 :stream stream)
                (incf newx 12)) )
             (if tuple (draw-tuple-number (+ x 1) (+ y 7) tuple :stream stream))
             )
            ((equal direction :down)
             (gp-move-to x (+ y 2) :stream stream)
             (case note             ;stem dowm
               (1 (put-char 119 :stream stream)
                (incf newx 60))
               (2 (put-char 72 :stream stream)
                (incf newx 45))
               (4 (put-char 81 :stream stream)
                (incf newx 25))
               (8 (put-char 69 :stream stream)
                (incf newx 15))
               (16 (put-char 88 :stream stream)
                (incf newx 12))
               (32 (put-char 82 :stream stream)
                (incf newx 12))
               (64 (put-char 239 :stream stream)
                (incf newx 12))
               (t (gp-move-to x (+ y 1) :stream stream)
                  (put-char 192 :stream stream)
                (incf newx 12)))
             (if tuple (draw-tuple-number (+ x 1) (- y 8) tuple :stream stream)) ))
      (cond (dot
              (gp-move-to (+ 8 x) y :stream stream)
              (put-char 46 :stream stream) ))
      newx ))

;(draw-one-note 100 50 "C" 5 64 t)


;; ------------
;;  DRAW-ONE-NOTE-OR-CHORD
;; ------------
;;
#|
(defun draw-one-note-or-chord (x ye n dot &key stream)
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
                         (toneoctave-to-octave (car tone)) note dot :stream stream :direction direction) )
            (dolist (tone (cdr tone))
               (draw-one-note x ye (toneoctave-to-tone tone)
                 (toneoctave-to-octave tone) 
                 (if (< note 8) note 4)
                 dot :stream stream :direction direction
                 )
               ))
         
         ;ONLY ONE NOTE
         (setq newx (draw-one-note x ye (toneoctave-to-tone tone)
                      (toneoctave-to-octave tone) note dot :stream stream)
           ))
    newx ))
|#
(defun draw-one-note-or-chord (x ye n dot &key (tuple nil) stream)
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
                         (toneoctave-to-octave (car tone)) note dot :stream stream :direction direction) )
            (dolist (ton (cdr tone))
               (draw-one-note x ye (toneoctave-to-tone ton)
                 (toneoctave-to-octave ton) 
                 (if (< note 8) note 4)
                              dot :tuple (if (string-equal ton (car (last tone))) tuple nil)
                              :stream stream :direction direction
                 )))        
         ;ONLY ONE NOTE
         (setq newx (draw-one-note x ye (toneoctave-to-tone tone)
                      (toneoctave-to-octave tone) note dot :tuple tuple :stream stream)
           ))
     newx ))


;; ------------
;;  DRAW-NOTE
;; ------------
;;

;including new notevalue format used by load-midifile /af
(defun draw-note (x ye n dot prop-val-list &key (tuple nil) stream)
   (let ((tone (car n))
         (note (cdr n))
         (newx) )
      (if (listp note) ;new list format
         (let ((i 0) (dx 0))
            (untilexit loop
              (if (>= i (length note)) (return-from loop))
              (cond 
                    ((and (< i (1- (length note)))           ;print dotted
                          (= 2 (/ (nth i note) (nth (1+ i) note))) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (denominator (nth i note))) t :stream stream)
                     (incf i)
                     (incf dx 10) )
                    ((and (= 3 (numerator (nth i note)))           ;print dotted
                          (member (denominator (nth i note)) '(1 2 4 8 16 32 64)) )
                     (draw-one-note-or-chord (+ x dx) ye 
                       (cons tone (/ (denominator (nth i note)) 2)) t :stream stream)
                     (incf dx 10) )
                    ((and (= 1 (numerator (nth i note)))           ;triplets
                          (member (denominator (nth i note)) '(3 6 12 24 48)) )
                     (draw-one-note-or-chord 
                      (+ x dx) ye (cons tone (/ (* (denominator (nth i note)) 2) 3)) nil :tuple 3 :stream stream)
                     ;(draw-tuple-number x ye 3 :stream stream)
                     (incf dx 10) )
                    (t 
                      (dotimes (numerator (nth i note))
                         (draw-one-note-or-chord (+ x dx) ye 
                           (cons tone (denominator (nth i note))) nil :stream stream)
                         (incf dx 10) ))
                    )
              (incf i)
              ))
         (draw-one-note-or-chord x ye n dot :tuple tuple :stream stream) ; old dot format
         )
      (if prop-val-list
         (print-val-list x (+ ye 4) prop-val-list :stream stream)
         )
     ))


;; ------------
;;  PIANO ROLL STYLE 
;; ------------
;;

      
(defun draw-one-roll-bar (x dx ye f0 &key stream)
  (let* ((posnr (posnr-f0 f0))
          (y (- ye (round (*  2 (- posnr 2)))))
          (newx x) )
      (cond 
            ((sharp-f0? f0)
             (gp-move-to (+ x -6) (+ y 5) :stream stream)
             (put-char 35 :stream stream) ))
    ;(draw-helplines x ye posnr :stream stream)
    (gp-move-to x y :stream stream)
    ;(gp-line-to (+ x dx) y :stream stream)
    (gp-draw-filled-box x (1- y) (max (+ x 2) (+ x dx -1)) (+ y 2) :stream stream)
    ))

(defun draw-roll-bar (x dx ye f0 prop-val-list &key stream)
  (with-foreground-color (stream blue)
  (if (listp f0)
      (dolist (f0nr f0)
        (draw-one-roll-bar x dx ye f0nr :stream stream) )
    (draw-one-roll-bar x dx ye f0 :stream stream) )
  (if prop-val-list (print-val-list x (+ ye 4) prop-val-list :stream stream))
    ))

(defun draw-rest-roll-bar (x dx ye prop-val-list &key stream)
  (with-foreground-color (stream gray)
    (gp-move-to x ye :stream stream)
    ;(gp-line-to (+ x dx) y :stream stream)
    ;(draw-box stream (make-box x (- ye 16) (max (+ x 2) (+ x dx -2)) (+ ye 0)))
    ;(gp-draw-filled-box x (- ye 16) (max (+ x 2) (+ x dx -2)) (+ ye 0) :stream stream)
    (gp-draw-filled-box x (- ye 10) (max (+ x 2) (+ x dx -2)) (- ye 6) :stream stream)
    )
  (if prop-val-list (print-val-list x (+ ye 4) prop-val-list :stream stream))
    )

;; -----------------
;;  PRINT-VAL-LIST
;; -----------------
;;
(defun print-val-list (x y list &key stream)
   (let ((dy 8)(y-start 2))
      (gp-select-text-font :stream stream)
      (dolist (val list)
         (gp-move-to x (+ y (incf dy 10) y-start) :stream stream)
         (if val (gp-write-string (princ-to-string val) :stream stream)) )
      (gp-select-music-font :stream stream) ))


;; -----------------
;;  DRAW-HELPLINES
;; -----------------
;;

(defun draw-helplines (x ye posnr &key stream)
   (cond ((> posnr 11)
          (gp-move-to (- x 1) (+ ye -20) :stream stream)
          ;(print posnr)
          ;(if (oddp posnr) (gp-move 0 2 :stream stream))
          (loop for i from 0 to (truncate (/ (- posnr 12) 2.)) do
            (gp-line 8 0 :stream stream)
            (gp-move -8 -4 :stream stream) ))
         ((< posnr 1)
          (gp-move-to (- x 1) (+ ye 4) :stream stream)
          (loop for i from 0 to (truncate (/ (- posnr) 2.)) do
            (gp-line 8 0 :stream stream)
            (gp-move -8 4 :stream stream) ))
         ))

;; --------
;;  POSNR
;; --------
;;
;; relative to C4
;;
(defun posnr (tone oct)
 (+
  (* (- oct 4) 7) 
  (case (char tone 0)
    (#\C 0)(#\D 1)(#\E 2)(#\F 3)(#\G 4)(#\A 5)(#\B 6) )))

(defun posnr-f0 (f0)
  (setq f0 (round f0))
  (multiple-value-bind (oct tone) (floor f0 12)
    (+
     (* (- oct 5) 7) 
     (case tone
       (0 0)(1 0)(2 1)(3 1)(4 2)(5 3)(6 3)(7 4)(8 4)(9 5)(10 5)(11 6) ))))

(defun sharp-f0? (f0)
  (member (mod f0 12) '(1 3 6 8 10)) )


;; --------
;;  Y-POS 
;; --------
;;
;; relative ye     to-clip 2.1 for the sonata barlines
;;
#|
(defun y-pos (ye nr-rel-c)
  (+ ye 0 (- (round (* (if (get-dm-var 'toclip?) 2 2) (- nr-rel-c 2))))) )
|#
;;for anastasia
(defun y-pos (ye nr-rel-c)
  (+ ye 4 (- (round (* (if (get-dm-var 'toclip?) 2 2) (- nr-rel-c 2))))) )

;;;for MusicalSymbols - not yet good
;;;(defun y-pos (ye nr-rel-c)
;;;    (+ ye 3 (- (round (* (if (get-dm-var 'toclip?) 2 2) (- nr-rel-c 2))))) )


;; -----------
;;  DRAW-BAR
;; -----------
;;
(defun draw-bar (x ye &key stream)
   (gp-move-to x ye :stream stream)
   (gp-line 0 -16 :stream stream)
   ;(incf x 2) 
   2 )

;; -------------
;;  draw-music-lines
;; -------------
;;
(defun draw-music-lines (x1 x2 ye &key stream)
   (loop for i from 0 downto -16 by 4 do     ; careful: the step is actually "-4", not really 4
     (gp-move-to x1 (+ i ye) :stream stream)
     (gp-line-to x2 (+ i ye) :stream stream)
     ))

;; ---------
;;  DRAW-G
;; ---------
;;
#|
(defun draw-g (x ye &key stream)
   (gp-move-to x ye :stream stream)
   (put-char 38 :stream stream)
  20 )
|#
(defun draw-g (x ye &key stream)
   (gp-move-to x (+ ye 6) :stream stream)
   (put-char 38 :stream stream)
   20 )
;; ===========================
;;   BASIC GRAPHIC FUNCTIONS
;; ===========================

(defun put-char (char &key stream)
   (move-by stream (make-position 0 -32))
   (princ (cltl1:int-char char) stream)
   (move-by stream (make-position 0 32))
   )

(defun gp-erase-rect (left top right bot &key stream)
   (erase-box stream (make-box left top right bot))
   )

(defun gp-move-to (h v &key stream)
   (move-to stream (make-position h v))
   )

(defun gp-move (h v &key stream)
   (move-by stream (make-position h v))
   )

(defun gp-line-to (h v &key stream)
   (draw-to stream (make-position h v))
   )

(defun gp-line (h v &key stream)
   (draw-by stream (make-position h v))
  )

(defun gp-draw-filled-box (left top right bot &key stream)
  (fill-box stream (make-box left top right bot)))

(defun gp-draw-box (left top right bot &key stream)
  (draw-box stream (make-box left top right bot)))

(defun gp-paint-oval ( left top right bot &key stream)
   )

(defun gp-write-string (string &key stream)
   ;  (draw-string-in-box stream string 0 (lenght string) (makebox ) :left :top true
   (move-by stream (make-position 0 -8)) 
   (princ string stream)     ;      ***** with-pstrs in MAC version
   (move-by stream (make-position 0 8))
   )


;;eof
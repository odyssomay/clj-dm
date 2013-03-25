 ;;;-*-Mode: LISP; Package: DM -*-
;;
;; *************************************************
;;   Loading/saving scores and printing properties
;; *************************************************
;;
;; print/read the music properties to/from a file
;; 8610 Anders Friberg
;; 8809 -- new function names and dialogs, any filepath permitted
;; 9702/vc Mac/Win95 version
;; 9703/af added io for the whole score object
;; 9812/af added track vars to the usual .mus and .per format
;; 000925/af add checking for missing track slots

;; NEW FORMAT
;; <track-type>   ;first track
;; :<track-var-name> <track-var-value>
;; :<track-var-name> <track-var-value>
;; :<track-var-name> <track-var-value>
;; ...
;; <segment-list>
;; <segment-list>
;; <segment-list>
;; <segment-list>
;; ...
;; <track-type>   ;second track
;; :<track-var-name> <track-var-value>
;; :<track-var-name> <track-var-value>
;; :<track-var-name> <track-var-value>
;; ...
;; <segment-list>
;; <segment-list>
;; <segment-list>
;; <segment-list>
;; ...
;; EXAMPLE:
;;; mono-track
;;;  :trackname "V1"
;;;  :midi-channel 1
;;;  :midi-initial-volume 0
;;;  :midi-initial-program 1
;;;  :synth "PINNACLE"
;;; (ph "E:" bar 1 n ("G3" . 4) dot 1 meter (4 4) mm 187 q
;;;  ("G" "B" "D") modus "maj" key "G" phrase t phrase-start (4 5 6))
;;; (ph "Å" n ("A3" . 8))
;;; (ph "A:" n ("B3" . 4))


(in-package :dm)


; the following functions are defined
;   load-score ()
;   load-score-fpath (fpath)
;   save-score ()
;   save-score-fpath (fpath)
;  rem-play-prop ()
;  print-music (&optional ofile)
;  print-music-round ()
;  print-music-prop (&rest plist)

;; --------------------------------------------------
;; ------   L O A D I N G     M U S I C   -----------
;; --------------------------------------------------

;; --------------       
;;   LOAD-SCORE
;; --------------
;; 

#-:lispworks
(defun load-score ()
   (let ((fpath (show-dialog-for-opening-files-PD "Load score"
                 :directory (get-dm-var 'music-directory)
                 :extensions '(("MUS score files" . "*.mus")("All files" . "*.*")) )))
      (if fpath (with-waiting-cursor 
                  (read-active-score-from-file fpath)
                  (set-dm-var 'music-directory (directory-namestring fpath))
                  (setf (nickname *active-score*) (file-namestring fpath))
                  (make-or-update-edit-music-window) ;def in musicdialog
                  (init-music)
                  (redraw-display-windows) ;def in drawProp
                  (redraw-music-windows) ;def in drawPolyNotes
                  ))
     ))

#+:lispworks
(defun load-score ()
   (let ((fpath (show-dialog-for-opening-files-PD "Load score"
                 :directory (get-dm-var 'music-directory)
                 :extensions '(("MUS score files" . "*.mus")("All files" . "*.*")) )))
      (if fpath (with-waiting-cursor 
                  (read-active-score-from-file fpath)
                  (set-dm-var 'music-directory (directory-namestring fpath))
                  (setf (nickname *active-score*) (file-namestring fpath))
                 ; (make-or-update-edit-music-window) ;def in musicdialog
                  (init-music)
                 ; (redraw-display-windows) ;def in drawProp
                 ; (redraw-music-windows) ;def in drawPolyNotes
                  ))
     ))

;;just as above but with file input
(defun load-score-fpath (fpath)
     (with-waiting-cursor 
                  (read-active-score-from-file fpath)
                  (set-dm-var 'music-directory (directory-namestring fpath))
                  (setf (nickname *active-score*) (file-namestring fpath))
                  (make-or-update-edit-music-window) ;def in musicdialog
                  (init-music)
                  (redraw-display-windows) ;def in drawProp
                  (redraw-music-windows) ;def in drawPolyNotes
                  ))
   
;; --------------------       
;;   LOAD-PERFORMANCE
;; --------------------
;; 
(defun load-performance ()
   (let ((fpath (show-dialog-for-opening-files-PD "Load performance"
                 :directory (get-dm-var 'music-directory)
                 :extensions '(("Performance files" . "*.per")("All files" . "*.*")) )))
      (if fpath (with-waiting-cursor 
                  (read-active-score-from-file fpath)
                  (set-dm-var 'music-directory (directory-namestring fpath))
                  (setf (nickname *active-score*) (file-namestring fpath))
                  (make-or-update-edit-music-window) ;def in musicdialog
                  ;(init-music)
                  (redraw-display-windows) ;def in drawProp
                  (redraw-music-windows) ;def in drawPolyNotes
                  ))
      ))

(defun read-active-score-from-file (fpath)
   (let ((inlist)
         (score (make-instance 'score :score-filename fpath))
         (track)
         (old-format-p nil))
      (with-open-file (ifile fpath :direction :input )
        (setq inlist (read ifile nil))
        
        (cond 
         ((symbolp inlist)  ;first symbol
          (case  inlist
            (mono-track (setq track (make-instance 'mono-track)))
            (voice-track (setq track (make-instance 'voice-track)))
            (t (setq old-format-p t)   ;old format
               (setq track (make-instance 'mono-track :trackname (string inlist)))) )
          (loop             ;the rest
            (setq inlist (read ifile nil))
            (cond ((not inlist)       ;end of file
                   (add-one-track score track)
                   (return) )
                  ((keywordp inlist)      ;track var
                   (cond ((slot-exists-p track (read-from-string (string inlist)))
                          (setf (slot-value track (read-from-string (string inlist)))
                            (case inlist 
                              (:synth (make-synth (read ifile nil)))
                              (t (read ifile nil))) ))
                         (t (read ifile nil)
                            (warn "not a track variable: ~A" inlist))))
                  ((symbolp inlist)      ;new track
                   (add-one-track score track)
                   (case  inlist
                     (mono-track (setq track (make-instance 'mono-track)))
                     (voice-track (setq track (make-instance 'voice-track)))
                     (t (setq track (make-instance 'mono-track :trackname (string inlist))))  ;old format
                     ))
                  ((listp inlist)              ;if list new tone
                   (add-one-segment track (make-instance 'segment :var-list (list-to-alist inlist))))
                  (t (error "Not expected in file: ~A" inlist)) )))
         (t (error "Not expected in file: ~A" inlist)) )               
                 )
      (setq *active-score* score)
     (if old-format-p (get-track-par))
     (recreate-time-shapes)
      (if (get-dm-var 'verbose-i/o) (print-ll  "Active score loaded from " fpath))
      score))

;;same as above but reads from a string input
;;used in eg socket communication
(defun read-active-score-from-string (string)
   (let ((inlist)
         (score (make-instance 'score :score-filename "no name"))
         (track)
         (old-format-p nil)
         (i 0)  ;index into the string
         (dummy nil))
     ; (with-open-file (ifile fpath :direction :input )
     ;  (setq inlist (read ifile nil))
        (multiple-value-setq (inlist i) (read-from-string string nil nil :start i))
        (cond 
         ((symbolp inlist)  ;first symbol
          (case  inlist
            (mono-track (setq track (make-instance 'mono-track)))
            (voice-track (setq track (make-instance 'voice-track)))
            (t (setq old-format-p t)   ;old format
               (setq track (make-instance 'mono-track :trackname (string inlist)))) )
          (loop             ;the rest
            ;(setq inlist (read ifile nil))
            (multiple-value-setq (inlist i) (read-from-string string nil nil :start i))
            (cond ((not inlist)       ;end of file
                   (add-one-track score track)
                   (return) )
                  ((keywordp inlist)      ;track var
                   (cond ((slot-exists-p track (read-from-string (string inlist)))
                          (setf (slot-value track (read-from-string (string inlist)))
                            (case inlist 
                              (:synth (make-synth (multiple-value-setq (dummy i) (read-from-string string nil nil :start i))))
                              (t (multiple-value-setq (dummy i) (read-from-string string nil nil :start i)))) ))
                         (t (multiple-value-setq (dummy i) (read-from-string string nil nil :start i))
                            (warn "not a track variable: ~A" inlist))))
                  ((symbolp inlist)      ;new track
                   (add-one-track score track)
                   (case  inlist
                     (mono-track (setq track (make-instance 'mono-track)))
                     (voice-track (setq track (make-instance 'voice-track)))
                     (t (setq track (make-instance 'mono-track :trackname (string inlist))))  ;old format
                     ))
                  ((listp inlist)              ;if list new tone
                   (add-one-segment track (make-instance 'segment :var-list (list-to-alist inlist))))
                  (t (error "Not expected in file: ~A" inlist)) )))
         (t (error "Not expected in file: ~A" inlist)) )               
                 ;)
      (setq *active-score* score)
     (if old-format-p (get-track-par))
     (recreate-time-shapes)
     (if (get-dm-var 'verbose-i/o) (print-ll  "Active score loaded from string" ))
      score))

;;transfer track parameters from notes and removes on the notes
;;used for old file format and midifileinput
(defun get-track-par ()
      (each-note-if
       (first?)
       (then
        (when (this 'pr) (setf (midi-initial-program *this-track*) (this 'pr)) (rem-this 'pr))
        (when (this 'channel) (setf (midi-channel *this-track*) (this 'channel)) (rem-this 'channel))
        (when (this 'vol) (setf (midi-initial-volume *this-track*) (this 'vol)) (rem-this 'vol))
        (when (not (last?))
          (when (next 'pr) (setf (midi-initial-program *this-track*) (next 'pr)) (rem-next 'pr))
          (when (next 'channel) (setf (midi-channel *this-track*) (next 'channel)) (rem-next 'channel))
          (when (next 'vol) (setf (midi-initial-volume *this-track*) (next 'vol)) (rem-next 'vol))
          (cond ((this 'synt) 
                 (setf (synth *this-track*) (make-synth (this 'synt)))
                 (rem-this 'synt) )
                (t  (setf (synth *this-track*) (make-synth (get-dm-var 'play-synth-name-default)))) ))
        
        (exit-track)
          )))

;;create time-shape objects with breakpoints as specified in lists: (<time> <value> ....)
(defun RECREATE-TIME-SHAPES ()
  (each-segment
   (if (and (this 'va) (listp (this 'va)))
          (set-this 'va (THIS-NOTE-MAKE-TIME-SHAPE :interpolation :linear :break-point-list (this 'va))) )
    (if (and (this 'vf) (listp (this 'vf)))
          (set-this 'vf (THIS-NOTE-MAKE-TIME-SHAPE :interpolation :linear :break-point-list (this 'vf))) )
    (if (and (this 'vol) (listp (this 'vol)))
          (set-this 'vol (THIS-NOTE-MAKE-TIME-SHAPE :interpolation :linear :break-point-list (this 'vol))) )
    (if (and (this 'dc) (listp (this 'dc)))
          (set-this 'dc (THIS-NOTE-MAKE-TIME-SHAPE :interpolation :linear :break-point-list (this 'dc))) )
          ))


;; ---------------------------------------------------------
;; -----    S A V I N G    M U S I C     -------------------
;; ---------------------------------------------------------

;; --------------
;;   SAVE-SCORE
;; --------------
;;
;; save the music with "save as" dialog
;; will init some prop on the menu; it's called from the menu
;;
(defun save-score ()
   (let ((fpath (show-dialog-for-saving-files-PD "Save score"
                 :directory (merge-pathnames (make-pathname :type "mus") (score-filename *active-score*))
                 :extensions '(("MUS score files" . "*.mus")("All files" . "*.*")) )))
     (if fpath (with-waiting-cursor 
                 (save-score-fpath fpath)
                 (setf (score-filename *active-score*)  fpath)
                 )
       (print "I could not save the file") )) 
   )

;; --------------------
;;   SAVE-SCORE-FPATH
;; --------------------
;;
;; called by save-score
;;
; **** right now it kills and then restore the play properties
; --> list of prop; list composed only by score properties.
(defun save-score-fpath (fpath)
   (rem-play-prop)
   (with-open-file (ofile fpath :direction :output 
                     :if-does-not-exist :create
                     :if-exists :supersede)
     (print-to-file *active-score* ofile))
   (init-music)
   (if (get-dm-var 'verbose-i/o) (print-ll "Active score saved in " fpath)) )


;; --------------------
;;   SAVE-PERFORMANCE
;; --------------------
;;
;; print to a file all properties. It's called from the menu
;;
(defun save-performance ()
   (let ((fpath (show-dialog-for-saving-files-PD "Save performance"
                 :directory (merge-pathnames (make-pathname :type "per")
                              (score-filename *active-score*)) 
                 :extensions '(("Performance files" . "*.per")("All files" . "*.*")) )))
      
      ;;;   :directory (merge-pathnames  ".per" (or (get-filename) "temp"))))    **** study carefully
      
     (if fpath (with-waiting-cursor
                (save-performance-fpath fpath)   
                (setf (score-filename *active-score*)  fpath)
                ) 
       (print "I could not save the file") )))


(defun save-performance-fpath (fpath)
  (with-open-file (ofile fpath :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
    (let ((print-right-margin 80))
      (print-to-file *active-score* ofile) ))
  (if (get-dm-var 'verbose-i/o)
     (print-ll "Active score saved as performance in " fpath))
  )

;; --------------
;;   SAVE-PERFORMANCE-OBJECT
;; --------------
;;
;; save the score object in its current state
;; not working any longer

#|
(defun save-performance-object ()
   (let ((fpath (show-dialog-for-saving-files-PD "Save score object"
                 :directory (score-filename *active-score*)
                 :extensions '(("MUS score files" . "*.obj")("All files" . "*.*")) )))
      (if fpath (save-performance-object-fpath fpath)) ))
         
(defun save-performance-object-fpath (fpath)
  (with-open-file (ofile fpath :direction :output 
                         :if-does-not-exist :create
                         :if-exists :supersede)
    (let ((print-right-margin 100) ;funkar ej
          )
      (pprint *active-score* ofile) ))
  (if (get-dm-var 'verbose-i/o) 
       (print-ll "Active score saved as object in " fpath))
  )
|#

;; -----------------
;;   REM-PLAY-PROP
;; -----------------
;;
;; ******* to change completely the mechanism: Provide a properties list as a filter
;;

(defun rem-play-prop ()
  (each-track-all-tracks
  (each-note
    (rem-this 'sl)
    (rem-this 'dr)
    (rem-this 'dro)
    (rem-this 'ndr)
    (rem-this 'f0)
    (rem-this 'va)
    (rem-this 'vf)
    (rem-this 'dc)
    (rem-this 'vol)
    (rem-this 'env)
 )))

;; avoid using this more function, but remember to call redraw
;(defun redraw-display-windows ()) ;some fixes
;(defun redraw-music-windows ())


;; -------------------------------------------------------------
;; -----    P R I N T I N G    M U S I C     -------------------
;; -------------------------------------------------------------

;; ---------------
;;   PRINT-MUSIC
;; ---------------
;;
;; simple utility, a better version to be developed
;;
(defun print-music (&optional ofile)
   (each-track 
     (print (trackname (nth *i* *v*)) ofile)
     (each-segment
       (print (alist-to-list (var-list (nth *i* *v*))) ofile)
       )))

;; (var-list --> the segment, a list of sub lists)
;; then alist-to-list remove the extra parenthesys

;; ---------------------
;;   PRINT-MUSIC-ROUND
;; ---------------------
;;
;; print all properties with float's rounded
;;

(defun print-music-round (&optional ofile)
   (each-track
     (print (trackname (nth *i* *v*)) ofile)
     (each-segment
          (print (round-list (alist-to-list (var-list (nth *i* *v*)))) ofile)
          )))

(defun round-list (list)
  (let ((outlist '()))
        (dolist (a list)
           (setq outlist 
                 (append outlist 
                         (list (if (floatp a)
                                 (/ (round (* 10.0 a)) 10.0) 
                                 (if (and (listp a) (listp (cdr a))) (round-list a) a)
                                   )))))
        outlist))


;prints only the properties in *print-prop-list*
;for the voices in *vlist*
;if arguments is given  then it prints these prop. instead
;ex: (print-music-prop) or (print-music-prop 'dr 'ndr 'n)
(defun print-music-prop (&rest plist)
  (let ((*print-prop-list* (if plist plist (get-dm-var 'print-prop-list))))
   (each-track
     (print (trackname (nth *i* *v*)))
     (each-segment
      (let ((l))
        (declare (special l))
        (mapc #'(lambda (prop)
                 (declare (special l))
                 (cond ((iget *i* prop)
                        (setq l (append1 l prop))
                        (setq l (append1 l (iget *i* prop))) )))
              *print-prop-list* )
        (print l) )))))

(defun print-music-prop-round (&rest plist)
  (let ((*print-prop-list* (if plist plist (get-dm-var 'print-prop-list))))
   (each-track
     (print (trackname (nth *i* *v*)))
     (each-segment
      (let ((l))
        (declare (special l))
        (mapc #'(lambda (prop)
                 (declare (special l))
                 (cond ((iget *i* prop)
                        (setq l (append1 l prop))
                        (setq l (append1 l (iget *i* prop))) )))
              *print-prop-list* )
        (print (round-list l)) )))))


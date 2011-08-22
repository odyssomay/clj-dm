;; Copyright (c) 2004 Lars Frydén, Johan Sundberg, Anders Friberg, Roberto Bresin
;; For information on usage and redistribution, and for a DISCLAIMER OF ALL
;; WARRANTIES, see the files, "README.txt" and "DIRECTORMUSICES.LICENSE.txt", in this distribution.

;;;------------ MIDIfile 0.04 output ------------------------------------------
;;;
;;;/Anders Friberg  October 1987.
;;;for CL 9112 
;;;midifile type 1 implemented 9310
;;;9810/af fixed roundoff error in midifile-write-list --puh!
;;;2000-10-20/af added trackname printing
;;;2002-10/af added save-performance-midifile1-stream for writing to sockets
;;;2003-05-20/af fixed a round off error in midifile-write-list
;;;2004-03/af added sysex write for midi type 1
;;;2004-05/rb added copyright notice write for midi type 1 and 0

(in-package :dm)

;;; moved to dm-var
;;;(defvar *midi-ticks*)
;;;(setq *midi-ticks* 240) ;used to be 240

;;;-----------------------------------
;;; from the menu
;;;-----------------------------------

#|
(defun save-performance-midifile0 ()
  (let ((fpath (show-dialog-for-saving-files-PD 
                "Save MIDI file type 0"
                :directory (merge-pathnames (make-pathname :type "mid") (score-filename *active-score*))
                :extensions '(("MIDI files" . "*.mid")("All files" . "*.*")) )))
    (when fpath
      #+:mcl
      (with-cursor *watch-cursor*
        (save-performance-midifile0-fpath fpath)
        (set-mac-file-type fpath "Midi") )
      #+:mswindows
        (save-performance-midifile0-fpath fpath)
      
      (setf (score-filename *active-score*)  fpath)
      )))
(defun save-performance-midifile1 ()
   (let ((fpath (show-dialog-for-saving-files-PD 
                 "Save as MIDI file type 1"
                 :directory (merge-pathnames (make-pathname :type "mid") (score-filename *active-score*))
                 :extensions '(("MIDI files" . "*.mid")("All files" . "*.*")) )))
      (when fpath   
         #+:mcl
         (with-cursor *watch-cursor*
           (save-performance-midifile1-fpath fpath)
           (set-mac-file-type fpath "Midi") )
         #+:mswindows
         (with-cursor (waiting-cursor)
            (save-performance-midifile1-fpath fpath) )
      
         (setf (score-filename *active-score*)  fpath)
        )))
|#

(defun save-performance-midifile0 ()
   (let ((fpath (show-dialog-for-saving-files-PD 
                 "Save as MIDI file type 0"
                 :directory (merge-pathnames (make-pathname :type "mid") (score-filename *active-score*))
                 :extensions '(("MIDI files" . "*.mid")("All files" . "*.*")) )))
     (when fpath
       (with-waiting-cursor
        (save-performance-midifile0-fpath fpath)
        #+:mcl
        (set-mac-file-type fpath "Midi")
        (setf (score-filename *active-score*)  fpath)
        ))))


(defun save-performance-midifile1 ()
   (let ((fpath (show-dialog-for-saving-files-PD 
                 "Save as MIDI file type 1"
                 :directory (merge-pathnames (make-pathname :type "mid") (score-filename *active-score*))
                 :extensions '(("MIDI files" . "*.mid")("All files" . "*.*")) )))
     (when fpath
       (with-waiting-cursor
        (save-performance-midifile1-fpath fpath)
        #+:mcl
        (set-mac-file-type fpath "Midi")
        (setf (score-filename *active-score*)  fpath)
         ))))

;;;-----------------------------------
;;; fpath toplevel versions
;;;-----------------------------------

(defun save-performance-midifile0-fpath (fpath)
  (let ((midi-cnl ())
        (last-time 0.0)
        (first-data? t)  ;to indicate the first midi data in 'write-list
        (meter (get-first 'meter))
        (mm (or (get-first 'mm) 187.5))
        (key (get-first 'key))
        (modus (get-first 'modus))
        (ticks (get-dm-var 'output-midi-tick-res))
        )
    (declare (special midi-cnl last-time first-data?))
    (let ((tempo-factor (/ ticks (/ (mm-to-microsec mm) 1000.))))
      (declare (special tempo-factor))
      (format t "tempo-factor  ~D" tempo-factor) 
      (with-open-file (outfile fpath :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist :create
                               :if-exists :supersede)
        (print-cnl (list (char-code #\M) (char-code #\T)
                         (char-code #\h) (char-code #\d)
                         0 0 0 6 0 0 0 1 0 ticks) outfile) ;header chunk


      ;----- copyright notice -------
      (if (copyrightnotice-string *active-score*)
	  (setq midi-cnl (append midi-cnl
			        '(0 #xff 2) (list (length (copyrightnotice-string *active-score*)))
						  (string-to-byte-list (copyrightnotice-string *active-score*))))
	  (if (get-dm-var 'verbose-i/o) (format t "~&no copyright notice written")))


	
        (print-cnl (list (char-code #\M) (char-code #\T)
                         (char-code #\r) (char-code #\k)) outfile)                    ;track chunk
        (if meter                                         ;time signature
            (setq midi-cnl 
                  (append midi-cnl 
                          (list 0 #xff #x58 4 (car meter)
                                (case (cadr meter) (4 2)(2 1)(8 3)(16 4)(1 0))
                                (if (equal '(6 8) meter) 36 24)
                                8 )))
          (if (get-dm-var 'verbose-i/o) (format t "~&no meter written")) )
        (if (and key modus)                               ;key signature
            (setq midi-cnl 
                  (append midi-cnl 
                          (list 0 #xff #x59 2 
                                (key-to-sharps key modus)
                                (if (string= modus "min") 1 0) )))
          (if (get-dm-var 'verbose-i/o) (format t "~&no key written")) )
        (if mm                                             ;tempo
            (setq midi-cnl 
                  (append midi-cnl 
                          '(0 #xff #x51 3)
                          (cdr (adr-to-cnl (float-to-adr (mm-to-microsec mm)))) ))
          (format t "~&no tempo written"))
        (if (sysex-list *active-score*)          ;sysex
            (dolist (sysex (sysex-list *active-score*))
              ;(print sysex)
              ;(print (append midi-cnl '(0 #xf0)  sysex))
              (setq midi-cnl (append midi-cnl '(0 #xf0) (varlength-cnl (length sysex)) sysex)) ))
        (playlist-midifile)
        (setq midi-cnl (append midi-cnl '(0 #xff #x2f 0)))    ;end of track
        (let* ((length (length midi-cnl)))                  ;length
          (print-cnl 
           (list (hi-byte (hi-word length)) (lo-byte (hi-word length))
                 (hi-byte length) (lo-byte length)) outfile)) 
        (print-cnl midi-cnl outfile)                ;write the list
        )))
  (if (get-dm-var 'verbose-i/o) (print-ll  "Saved as MIDIfile type 0 : " fpath))
  ;(format t "~&Saved as MIDIfile : ~A" fpath)
  )

#|
;;added printing o track name
(defun save-performance-midifile1-fpath (fpath)
  (let ((midi-cnl ())  ;the list of midi bytes?
        (last-time)
        (first-data? nil)  ;to indicate the first midi data in 'write-list
        (meter (get-first 'meter))
        (mm (or (get-first 'mm) 187.5))
        (key (get-first 'key))
        (modus (get-first 'modus))
        (ticks (get-dm-var 'output-midi-tick-res))
        (nr-of-tracks (1+ (length (active-track-list *active-score*))))
         )
    (declare (special midi-cnl last-time first-data?))
    (let ((tempo-factor (/ ticks (/ (mm-to-microsec mm) 1000.))))
      (declare (special tempo-factor))
      (format t "tempo-factor  ~D" tempo-factor) 
      (with-open-file (outfile fpath :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist :create
                               :if-exists :rename-and-delete)
        
         ;---- print header chunk ---------
        (print-cnl (list (char-code #\M) (char-code #\T)
                           (char-code #\h) (char-code #\d)
                         0 0 0 6 0 1 0 nr-of-tracks 0 ticks) outfile)
        
         ;---- tempo track ---------
         (print-cnl (list (char-code #\M) (char-code #\T)
                           (char-code #\r) (char-code #\k)) outfile) 
          (if meter                                         ;time signature
            (setq midi-cnl 
                  (append midi-cnl 
                          (list 0 #xff #x58 4 (car meter)
                                (case (cadr meter) (4 2)(2 1)(8 3)(16 4)(1 0))
                                (if (equal '(6 8) meter) 36 24)
                                8 )))
            (format t "~&no meter written") )
          (if (and key modus)                               ;key signature
            (setq midi-cnl 
                  (append midi-cnl 
                          (list 0 #xff #x59 2 
                                (key-to-sharps key modus)
                                (if (string= modus "min") 1 0) )))
            (format t "~&no key written"))
          (if mm                                             ;tempo
            (setq midi-cnl 
                  (append midi-cnl 
                          '(0 #xff #x51 3)
                          (cdr (adr-to-cnl (float-to-adr (mm-to-microsec mm)))) ))
            (format t "~&no tempo written"))
          (setq midi-cnl (nconc midi-cnl '(0 #xff #x2f 0)))    ;end of track
          (let ((length (length midi-cnl)))                  ;length
            (print-cnl 
             (list (hi-byte (hi-word length)) (lo-byte (hi-word length))
                   (hi-byte length) (lo-byte length)) outfile)) 
        (print-cnl midi-cnl outfile)                ;write the list
        
         ;---- all other tracks ---------
          (each-track
              (setq midi-cnl ())
              ;(setq first-data? t)
              ;(setq first-data? nil)
              (setq last-time 0.0)
              ;(print-ll "midi-cnl " midi-cnl)
              (print-cnl (list (char-code #\M) (char-code #\T)           ;write header
                               (char-code #\r) (char-code #\k)) outfile)
              ;-- track name--- nconc doesn't work - then the old midi-cnl list will appear again????!!!!!
              (if (and (trackname *this-track*) (not (string= (trackname *this-track*) "")))  ;trackname
                (setq midi-cnl (append '(0 #xff #x03)
                                      (list (length (trackname *this-track*)))
                                      (string-to-byte-list (trackname *this-track*)) )))
              ;(print-ll "midi-cnl " midi-cnl)
              ;(print last-time)
                                    
              (let ((*active-score* (make-instance 'score :track-list (list *this-track*))))
                (playlist-midifile))
              (setq midi-cnl (nconc midi-cnl '(0 #xff #x2f 0)))    ;end of track
              ;(print midi-cnl)
              (let ((length (length midi-cnl)))                  ;length
                (print-cnl 
                 (list (hi-byte (hi-word length)) (lo-byte (hi-word length))
                       (hi-byte length) (lo-byte length)) outfile)) 
              (print-cnl midi-cnl outfile)                ;write the list
              )
              )))
  (format t "~&Saved as MIDIfile : ~A" fpath)
  )
|#

(defun save-performance-midifile1-fpath (fpath)
  (with-open-file (outfile fpath :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist :create
                           :if-exists :supersede)
    (save-performance-midifile1-stream outfile)
  (if (get-dm-var 'verbose-i/o) (print-ll  "Saved as MIDIfile type 1 : " fpath))
  ))

#|
(defun save-performance-midifile1-stream (stream)
  (let ((midi-cnl ())  ;the list of midi bytes?
        (last-time)
        (first-data? nil)  ;to indicate the first midi data in 'write-list
        (meter (get-first 'meter))
        (mm (or (get-first 'mm) 187.5))
        (key (get-first 'key))
        (modus (get-first 'modus))
        (ticks (get-dm-var 'output-midi-tick-res))
        (nr-of-tracks (1+ (length (active-track-list *active-score*))))
         )
    (declare (special midi-cnl last-time first-data?))
    (let ((tempo-factor (/ ticks (/ (mm-to-microsec mm) 1000.))))
      (declare (special tempo-factor))
      ;(format t "tempo-factor  ~D" tempo-factor) 
        
         ;---- print header chunk ---------
        (print-cnl (list (char-code #\M) (char-code #\T)
                           (char-code #\h) (char-code #\d)
                         0 0 0 6 0 1 0 nr-of-tracks 0 ticks) stream)
        
         ;---- tempo track ---------
         (print-cnl (list (char-code #\M) (char-code #\T)
                           (char-code #\r) (char-code #\k)) stream) 
          (if meter                                         ;time signature
            (setq midi-cnl 
                  (append midi-cnl 
                          (list 0 #xff #x58 4 (car meter)
                                (case (cadr meter) (4 2)(2 1)(8 3)(16 4)(1 0))
                                (if (equal '(6 8) meter) 36 24)
                                8 )))
            (format t "~&no meter written") )
          (if (and key modus)                               ;key signature
            (setq midi-cnl 
                  (append midi-cnl 
                          (list 0 #xff #x59 2 
                                (key-to-sharps key modus)
                                (if (string= modus "min") 1 0) )))
            (format t "~&no key written"))
          (if mm                                             ;tempo
            (setq midi-cnl 
                  (append midi-cnl 
                          '(0 #xff #x51 3)
                          (cdr (adr-to-cnl (float-to-adr (mm-to-microsec mm)))) ))
            (format t "~&no tempo written"))
          (setq midi-cnl (nconc midi-cnl '(0 #xff #x2f 0)))    ;end of track
          (let ((length (length midi-cnl)))                  ;length
            (print-cnl 
             (list (hi-byte (hi-word length)) (lo-byte (hi-word length))
                   (hi-byte length) (lo-byte length)) stream)) 
        (print-cnl midi-cnl stream)                ;write the list
        
         ;---- all other tracks ---------
          (each-track
              (setq midi-cnl ())
              ;(setq first-data? t)
              ;(setq first-data? nil)
              (setq last-time 0.0)
              ;(print-ll "midi-cnl " midi-cnl)
              (print-cnl (list (char-code #\M) (char-code #\T)           ;write header
                               (char-code #\r) (char-code #\k)) stream)
              ;-- track name--- nconc doesn't work - then the old midi-cnl list will appear again????!!!!!
              (if (and (trackname *this-track*) (not (string= (trackname *this-track*) "")))  ;trackname
                (setq midi-cnl (append '(0 #xff #x03)
                                      (list (length (trackname *this-track*)))
                                      (string-to-byte-list (trackname *this-track*)) )))
              ;(print-ll "midi-cnl " midi-cnl)
              ;(print last-time)
                                    
              (let ((*active-score* (make-instance 'score :track-list (list *this-track*))))
                (playlist-midifile))
              (setq midi-cnl (nconc midi-cnl '(0 #xff #x2f 0)))    ;end of track
              ;(print midi-cnl)
              (let ((length (length midi-cnl)))                  ;length
                (print-cnl 
                 (list (hi-byte (hi-word length)) (lo-byte (hi-word length))
                       (hi-byte length) (lo-byte length)) stream)) 
              (print-cnl midi-cnl stream)                ;write the list
              )
              ))
  ;(format t "~&Saved as MIDIfile : ~A" fpath)
  )
|#

(defun save-performance-midifile1-stream (stream)
  (let ((midi-cnl ())  ;the list of midi bytes?
        (last-time)
        (first-data? nil)  ;to indicate the first midi data in 'write-list
        (meter (get-first 'meter))
        (mm (or (get-first 'mm) 187.5))
        (key (get-first 'key))
        (modus (get-first 'modus))
        (ticks (get-dm-var 'output-midi-tick-res))
        (nr-of-tracks (1+ (length (active-track-list *active-score*))))
        )
    (declare (special midi-cnl last-time first-data?))
    (let ((tempo-factor (/ ticks (/ (mm-to-microsec mm) 1000.))))
      (declare (special tempo-factor))
      ;(format t "tempo-factor  ~D" tempo-factor) 
      
      ;---- print header chunk ---------
      (print-cnl (list (char-code #\M) (char-code #\T)
                       (char-code #\h) (char-code #\d)
                       0 0 0 6 0 1 0 nr-of-tracks 0 ticks) stream)
      
      ;---- tempo track ---------
      (print-cnl (list (char-code #\M) (char-code #\T)
                       (char-code #\r) (char-code #\k)) stream)

      ;----- copyright notice -------
      (if (copyrightnotice-string *active-score*)
	  (setq midi-cnl (append midi-cnl
			        '(0 #xff 2) (list (length (copyrightnotice-string *active-score*)))
						  (string-to-byte-list (copyrightnotice-string *active-score*))))
	  (if (get-dm-var 'verbose-i/o) (format t "~&no copyright notice written")))


      
      (if meter                                         ;time signature
          (setq midi-cnl 
                (append midi-cnl 
                        (list 0 #xff #x58 4 (car meter)
                              (case (cadr meter) (4 2)(2 1)(8 3)(16 4)(1 0))
                              (if (equal '(6 8) meter) 36 24)
                              8 )))
        (if (get-dm-var 'verbose-i/o) (format t "~&no meter written")) )
      (if (and key modus)                               ;key signature
          (setq midi-cnl 
                (append midi-cnl 
                        (list 0 #xff #x59 2 
                              (key-to-sharps key modus)
                              (if (string= modus "min") 1 0) )))
        (if (get-dm-var 'verbose-i/o) (format t "~&no key written")) )
      (if mm                                             ;tempo
          (setq midi-cnl 
                (append midi-cnl 
                        '(0 #xff #x51 3)
                        (cdr (adr-to-cnl (float-to-adr (mm-to-microsec mm)))) ))
        (if (get-dm-var 'verbose-i/o) (format t "~&no tempo written")) )
      (if (sysex-list *active-score*)          ;sysex
         (dolist (sysex (sysex-list *active-score*))
           ;(print sysex)
           ;(print (append midi-cnl '(0 #xf0)  sysex))
           (setq midi-cnl (append midi-cnl '(0 #xf0) (varlength-cnl (length sysex)) sysex)) )) ;
      
      ;(setq midi-cnl (nconc midi-cnl '(0 #xff #x2f 0)))    ;end of track
      (setq midi-cnl (append midi-cnl '(0 #xff #x2f 0)))    ;end of track
      (let ((length (length midi-cnl)))                  ;length
        (print-cnl 
         (list (hi-byte (hi-word length)) (lo-byte (hi-word length))
               (hi-byte length) (lo-byte length)) stream)) 
      (print-cnl midi-cnl stream)                ;write the list
      
      ;---- all other tracks ---------
      (each-track
       (setq midi-cnl ())
       ;(setq first-data? t)
       ;(setq first-data? nil)
       (setq last-time 0.0)
       ;(print-ll "midi-cnl " midi-cnl)
       (print-cnl (list (char-code #\M) (char-code #\T)           ;write header
                        (char-code #\r) (char-code #\k)) stream)
       ;-- track name--- nconc doesn't work - then the old midi-cnl list will appear again????!!!!!
       (if (and (trackname *this-track*) (not (string= (trackname *this-track*) "")))  ;trackname
           (setq midi-cnl (append '(0 #xff #x03)
                                  (list (length (trackname *this-track*)))
                                  (string-to-byte-list (trackname *this-track*)) )))
       ;(print-ll "midi-cnl " midi-cnl)
       ;(print last-time)
       
       (let ((*active-score* (make-instance 'score :track-list (list *this-track*))))
         (playlist-midifile))
       (setq midi-cnl (nconc midi-cnl '(0 #xff #x2f 0)))    ;end of track
       ;(setq midi-cnl (nconc midi-cnl '(127 #xff #x2f 0)))    ;end of track
       ;(print midi-cnl)
       (let ((length (length midi-cnl)))                  ;length
         (print-cnl 
          (list (hi-byte (hi-word length)) (lo-byte (hi-word length))
                (hi-byte length) (lo-byte length)) stream)) 
       (print-cnl midi-cnl stream)                ;write the list
       )
      ))
  ;(format t "~&Saved as MIDIfile : ~A" fpath)
  )



;;;-----------------------------------
;;; utilities
;;;-----------------------------------

(defun mm-to-microsec (mm) (* (/ 1. (/ mm 60.)) 1E6))

;(defun foo (mm) (adr-to-cnl (float-to-adr (mm-to-microsec mm))))

(defun key-to-sharps (key modus)
  (cond 
   ((string= modus "min") 
    (cond 
     ((string= key "A") 0)((string= key "D") #xFF)
     ((string= key "G") #xFE)((string= key "C") #xFD)
     ((string= key "F") #xFC)((string= key "Bb") #xFB)
     ((string= key "Eb") #xFA)((string= key "Ab") #xF9) 
     ((string= key "E") 1)((string= key "B") 2)
     ((string= key "F#") 3)((string= key "C#") 4)
     ((string= key "G#") 5)((string= key "D#") 6)
     ((string= key "A#") 7)
     (t (error (format nil "not valid key: ~A" key))) ))
   ((string= modus "maj") 
     (cond
      ((string= key "C") 0)((string= key "F") #xFF)
      ((string= key "Bb") #xFE)((string= key "Eb") #xFD)
      ((string= key "Ab") #xFC)((string= key "Db") #xFB)
      ((string= key "Gb") #xFA)((string= key "B") #xF9) 
      ((string= key "G") 1)((string= key "D") 2)
      ((string= key "A") 3)((string= key "E") 4)
      ((string= key "B") 5)((string= key "F#") 6)
      ((string= key "C#") 7) 
      (t (error (format nil "not valid key: ~A" key))) ))
   (t (error (format nil "not valid modus: ~A" modus))) ))

;;writes a list of bytes to file
(defun print-cnl (l outfile)
   (dolist (byte l)
     (write-byte byte outfile) ))

;converts a string to a list of bytes
;strings longer than 127 chars will be truncated
(defun string-to-byte-list (string)
   (let ((len (length string))
         (str-list '()))
      (if (> len 127) (setq len 127))
      (dotimes (i len)
         (newr str-list (char-code (char string i))))
      str-list))

;for debugging
#|
(defun print-cnl (l outfile)
  (declare (ignore outfile))
  (let ((*print-base* 16))
    (print l)))
|#

;;--- output utility functions ------------------

#| not used
(defun printvarlength (adr outfile)
  (let ((buffer))
   (setq buffer (logand adr #x7f))
   (while (> (setq adr (ash adr -7)) 0)
     (setq buffer (ash buffer 8))
     (setq buffer (logior buffer #x80))
     (setq buffer (+ buffer (logand adr #x7f)))
     )
   (untilexit loop
     (write-byte (cdr buffer) outfile)
     (ifn (equal (logand buffer #x80) 0)
        (setq buffer (ash buffer -8))
        (return-from loop) )) 
   nil
    ))
|#

(defun varlength-cnl (adr)
  (let ((buffer)(cnl ()))
   (setq buffer (logand adr #x7f))
   (loop while (> (setq adr (ash adr -7)) 0) do
     (setq buffer (ash buffer 8))
     (setq buffer (logior buffer #x80))
     (setq buffer (+ buffer (logand adr #x7f)))
     )
   (untilexit loop
     (newr cnl (logand buffer #xff))
     (if (not (equal (logand buffer #x80) 0))
        (setq buffer (ash buffer -8))
        (return-from loop) )) 
   cnl
    ))

;;called from midi-write-list

#|
(defun midifile-write-list (midil time)
  (declare (special midi-cnl last-time tempo-factor first-data?))
  ;(print-ll midil time)
  (if first-data?
    (progn 
      (setq last-time time)
      (setq first-data? nil)))
  (setq midi-cnl 
        (nconc midi-cnl 
                (varlength-cnl
                 (round 
                  (* (- time last-time) tempo-factor) ))
                midil ))
  (setq last-time time) )
(defun midifile-write-list (midil time)
  (declare (special midi-cnl last-time tempo-factor first-data?))
  ;(print-ll "midil " midil " time " time)
;;;   (if first-data?
;;;     (progn 
;;;       (setq last-time time)
;;;       (setq first-data? nil)))
  (multiple-value-bind (dticks dticksdec) 
      (round (* (- time last-time) tempo-factor))
     (setq midi-cnl (nconc midi-cnl (varlength-cnl dticks) midil ))
     (setq last-time (- time (/ dticksdec tempo-factor))) )  ;save round off remainder
  )
|#

;;fix since the dticks sometimes is -1 due to round off problems
;;otherwise probably a large value is obtained from the varlength-cnl
(defun midifile-write-list (midil time)
  (declare (special midi-cnl last-time tempo-factor first-data?))
  ;(print-ll "midil " midil " time " time)
  (multiple-value-bind (dticks dticksdec) 
      (round (* (- time last-time) tempo-factor))
    (when (minusp dticks) (incf dticksdec dticks) (setq dticks 0)) ;fix when dticks is negative
    ;(print-ll "last-time " last-time " tempo-factor " tempo-factor " dticks " dticks " dticksdec " dticksdec)
    (setq midi-cnl (nconc midi-cnl (varlength-cnl dticks) midil ))
    (setq last-time (- time (/ dticksdec tempo-factor))) )  ;save round off remainder
  )

;(defun printhexadr ((h . l)) (prinhex h)(prinhex l)(terpri))




;;----conversions 
      
(defun hi-byte (int) (logand (ash int -8) #xff)) 
(defun lo-byte (int) (logand int #xff))
(defun hi-word (int) (logand (ash int -16) #xffff))
(defun lo-word (int) (logand int #xffff))
(defun adr-to-cnl (adr)
    (list (hi-byte (hi-word adr)) (lo-byte (hi-word adr))
          (hi-byte adr) (lo-byte adr) ))

#|
;avrundar ej - klipper
(defun float-to-adr (float)
   (let* ((hi (truncate (/ float 65536.)))
          (lo-2 (/ (- float (* hi 65536.)) 2.))
          (lo) )
    (setq lo (ash (truncate lo-2) 1))
    (if (>= (- lo-2 (truncate lo-2)) 0.4)   ;fix the last bit
        (setq lo (logior lo 1)) )
    (cons hi lo)
    ))
|#
(defun float-to-adr (float)
   (round float))

;(defun foo (f) (float-to-adr f))







 
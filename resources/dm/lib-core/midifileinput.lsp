;; Copyright (c) 2004 Lars Frydén, Johan Sundberg, Anders Friberg, Roberto Bresin
;; For information on usage and redistribution, and for a DISCLAIMER OF ALL
;; WARRANTIES, see the files, "README.txt" and "DIRECTORMUSICES.LICENSE.txt", in this distribution.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Load-midifile loads a file into a midifile score which is an extended score object
;;  
;;  The input midifile is thougth to arrive from a score editor, ie not
;;  a performance. However, the reader accepts any note duration and
;;  will retain its original value. When it is saved loaded as a .mus file it 
;;  will be the nominal score durations instead obtained from the notation.
;;
;;  Notevalues are approximated from the tickdurations
;;
;;  Except for the noteons, most of the other midifile events are removed
;;
;;  Parts of the original midi parsing was written by Sven Emtell
;;
;;1998-03-03/Anders Friberg first version
;;2000-10-02/af added articulation threshold, dr quantization, and the dialog
;;2000-10-06/af corrected not recognized events so that ticks add up
;;2000-10-19/af added read-string, reads track names
;;2001-01-28/af Completely new quantizer of "grid" type similar to commercial sw
;;              assumes a fixed measure grid taken from the tempo and meter markings
;;              see below in the code for further comments
;;2003-05-03/af collect sysex
;;2004-03-14/af modified sysex put in score object new slot
;;2004-04-30/af-rb added channel split for type 0
;;2004-05-05/rb added copyright notice put in score
;;2004-09-21/af added midi-initial-volume and velocity (into nsl) read - back trans following sblive curve
;;2004-11-23/af added duration lower limit to 1 tick in midifile-mark-dr
;;2005-05-05/af added pitch bend read to 'dc assuming +-2 semitone range using staircase break point functions
;;2006-08-09/af ignore pitch bend before first note
;;2006-08-17/af chasing first program change to track var
;;;061003/af added bank select
;;;061018/af added reverb and pan
;;;120607/af removed list of fractions to be compatible with clj-dm


(in-package "DM")


;;internal parameters
(defvar *midifile-buffer*)          ;; Used by read-octet and peek-octet.
(defvar *mf-debug-info*)            ;; When set true, some debug info will be printed in the listener
                                    ;; during program run.
(setq *mf-debug-info* nil)    
;(setq *mf-debug-info* t)    

(defvar *guess-notevalues-p*)            ;; When set true, note values will be estimated
(setq *guess-notevalues-p* t)    ;; nil better for clj - it doesn't work that well anyway (120607/af)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  --- load a midifile ---
;;

(defun load-midifile ()                  
  (let ((fpath (show-dialog-for-opening-files-PD "Load MIDI"
                                                 :directory (get-dm-var 'music-directory)
                                                 :extensions '(("MIDI files" . "*.mid")("All files" . "*.*")) )))
    (when (and fpath (MIDIFILE-INPUT-DIALOG))
      (with-waiting-cursor
       (load-midifile-fpath fpath)
       (set-dm-var 'music-directory (directory-namestring fpath))
       (setf (nickname *active-score*) (file-namestring fpath))      
       (make-or-update-edit-music-window) ;def in musicdialog
       (redraw-display-windows) ;def in drawProp
       (redraw-music-windows) ;def in drawPolyNotes
       ))))

#|
(defun load-midifile-fpath (fpath)
   (let ((midifile (make-instance 'midifile :score-filename fpath)))
      (setq *active-score* midifile)
        (let ((chunk-size 0) (chunk-type ""))
           (declare (integer chunk-size n) (string chunk-type))
           (with-open-file (ifile (score-filename midifile)                   ;; input file by default
                             :element-type '(unsigned-byte 8))
                          
             (unless (string-equal "MThd" (read-string ifile 4 nil nil))
                (error "this is not a MIDI file - wrong header"))            ;; error

             (setq chunk-size (+ (ash (read-byte ifile nil nil) 24)         ;; header chunk-size might be
                                 (ash (read-byte ifile nil nil) 16)         ;; longer than 6 according to
                                 (ash (read-byte ifile nil nil) 8)          ;; Standard Midi Files 1.0
                                 (read-byte ifile nil nil)))                ;; (page 6)
             
             (setf (miditype midifile) (+ (ash (read-byte ifile nil nil) 8)
                                          (read-byte ifile nil nil)))
             (setf (ntrks midifile) (+ (ash (read-byte ifile nil nil) 8)
                                       (read-byte ifile nil nil)))
             (setf (division midifile) (+ (ash (read-byte ifile nil nil) 8)
                                          (read-byte ifile nil nil)))
             (if (get-dm-var 'verbose-i/o) (print-ll "MIDI file type: " (miditype midifile) " tracks: " (ntrks midifile)))
             
             (loop repeat (- chunk-size 6) do (read-byte ifile nil nil))    ;; cutting rest of header chunk
             
             (loop for i from 0 below (ntrks midifile) do           ;; taking care of tracks
               (setq chunk-type (read-string ifile 4 nil nil)
                     chunk-size (+ (ash (read-byte ifile nil nil) 24)
                                   (ash (read-byte ifile nil nil) 16)
                                   (ash (read-byte ifile nil nil) 8)
                                   (read-byte ifile nil nil)))
               
               (if (string= chunk-type "MTrk")
                  (progn
                    ;              (format t "~%chunk-type ~A" chunk-type)
                    (let ((track (make-instance 'midi-track)))
                       ;(print track)
                       (add-one-track midifile track)
                       ;(print *last-midifile*)
                       ;(print midifile)
                       (read-MTrk ifile track))
                    )
                  (progn
                    (format t "~%other chunk-type than MTrk!")
                    (loop repeat chunk-size do (read-byte ifile nil nil)))))
             ))
      (midifile-init)
      (get-track-par)
      (if (get-dm-var 'verbose-i/o) (print-ll  "Active score loaded from " fpath))
      midifile
     ))
|#
;;new version with type 0 split
(defun load-midifile-fpath (fpath)
  (let ((midifile (make-instance 'midifile :score-filename fpath)))
    (setq *active-score* midifile)
    (let ((chunk-size 0) (chunk-type ""))
      (declare (integer chunk-size) (string chunk-type))
      (with-open-file (ifile (score-filename midifile)                   ;; input file by default
                             :element-type '(unsigned-byte 8))
        
        (unless (string-equal "MThd" (read-string ifile 4 nil nil))
          (error "this is not a MIDI file - wrong header"))            ;; error
        
        (setq chunk-size (+ (ash (read-byte ifile nil nil) 24)         ;; header chunk-size might be
                            (ash (read-byte ifile nil nil) 16)         ;; longer than 6 according to
                            (ash (read-byte ifile nil nil) 8)          ;; Standard Midi Files 1.0
                            (read-byte ifile nil nil)))                ;; (page 6)
        
        (setf (miditype midifile) (+ (ash (read-byte ifile nil nil) 8)
                                     (read-byte ifile nil nil)))
        (setf (ntrks midifile) (+ (ash (read-byte ifile nil nil) 8)
                                  (read-byte ifile nil nil)))
        (setf (division midifile) (+ (ash (read-byte ifile nil nil) 8)
                                     (read-byte ifile nil nil)))
        (if (get-dm-var 'verbose-i/o) (print-ll "MIDI file type: " (miditype midifile) " tracks: " (ntrks midifile)))
        
        (loop repeat (- chunk-size 6) do (read-byte ifile nil nil))    ;; cutting rest of header chunk
        
        (loop for i from 0 below (ntrks midifile) do           ;; taking care of tracks
              (setq chunk-type (read-string ifile 4 nil nil)
                  chunk-size (+ (ash (read-byte ifile nil nil) 24)
                                (ash (read-byte ifile nil nil) 16)
                                (ash (read-byte ifile nil nil) 8)
                                (read-byte ifile nil nil)))
              
              (if (string= chunk-type "MTrk")
                  (cond ((= (miditype midifile) 0)    ;type 0
                         (loop for i from 1 to 16 do                     ;create 16 tracks one for each channel
                               (let ((track (make-instance 'midi-track)))
                                 (add-one-track midifile track)
                                 (setf (midi-channel track) i) ))
                         (add-one-track midifile (make-instance 'midi-track)) ;add one more for tempo stuff
                         (read-MTrk ifile nil)                           ;nil means take the channel number as track index
                         )
                        ((= (miditype midifile) 1)    ;type 1
                         (let ((track (make-instance 'midi-track)))
                           (add-one-track midifile track)
                           (read-MTrk ifile track))
                         ))
                (progn
                  (format t "~%other chunk-type than MTrk!")
                  (loop repeat chunk-size do (read-byte ifile nil nil)))))
        ))
    (midifile-init)
    (get-track-par)
    (if (get-dm-var 'verbose-i/o) (print-ll  "Active score loaded from " fpath))
    midifile
    ))

(defun midifile-init ()
  ;(each-segment (print *this-segment*))
  (midifile-mark-absticks)
  ;(midifile-mark-abstime)
  (when (get-dm-var 'midifile-input-quantize-dr) (midifile-quantize))
  (collect-notoffs-mark-ndrticks)
  (midifile-collect-bank-msb)
  (midifile-collect-bank-lsb)
  (midifile-collect-pan)
  (midifile-collect-reverb)
  (midifile-collect-programchange)
  (midifile-collect-pitch-bend)
  (midifile-collect-midivolume)
  ;(midifile-collect-sysex) ;collected direct in reading instead
  (midifile-collect-tempo-meter-key)
  (midifile-set-channel)
  (midifile-cleanup)
  (midifile-init-var)
  (midifile-collect-chords)
  (midifile-insert-rest)
  (midifile-insert-rest-last)
  (when (get-dm-var 'midifile-input-quantize-split-at-barlines)
    (midifile-split-rest)
    (midifile-split-note) )
  (midifile-mark-dr)
  ;(midifile-guess-notevalues)
  (if *guess-notevalues-p* (midifile-guess-notevalues-quant))
  ;(when (get-dm-var 'midifile-input-quantize-dr)(set-dur-from-tempo))
  (rebar)
  (rem-all 'absticks)
  ;(rem-all 'abstime)
  (rem-all 'ndrticks)
  )

#|
(defun midifile-mark-absticks ()
   (each-track
     (let ((absticks 0))
        (each-segment
          (incf absticks (delta-time *this-segment*))
          (set-this 'absticks absticks)
         ))))
|#

;;delta-time = absticks
(defun midifile-mark-absticks ()
        (each-segment
          (set-this 'absticks (delta-time *this-segment*))
          ))
   
(defun midifile-mark-abstime ()
   (let (tempo-factor) 
      (block tempoblock
        (each-segment-if
          (typep *this-segment* 'setTempo)
          (then
            (setq tempo-factor 
              (/ (/ (midi-tempo *this-segment*) 1000.0)
                 (division *active-score*)))
            (return-from tempoblock) )))
      (each-segment
        (set-this 'abstime (* tempo-factor (this 'absticks)))
       )))

#|
;;simple quant function
;;without triplets
(defun midifile-quantize ()
  (let ((quant-incr (* 4 (division *active-score*) 
                       (get-dm-var 'midifile-input-quantize-notevalues)))
         ) 
    (each-segment-if
     (this 'absticks)
     (then
     (let ((diff (mod (this 'absticks) quant-incr)))
       (if (<= diff (/ quant-incr 2.0))
           (add-this 'absticks (- diff))
         (add-this 'absticks (- quant-incr diff))
         ))))))
|#
;works???
;quantize function to two different grids
;takes two different minimum note vales, one for "normal" notes
;and one for triplet subdivision
;quantize to the shortest distance of any of the two
;note that this sometimes generates smaller note values
(defun midifile-quantize ()
  (let ((quant-incr (* 4 (division *active-score*) 
                       (get-dm-var 'midifile-input-quantize-notevalues)))
        (quant-incr3 (* 4 (division *active-score*)
                        (get-dm-var 'midifile-input-quantize-triplet-notevalues)))
        (add 1000))
    (print-ll "quant-incr " quant-incr "  quant-incr3 " quant-incr3)
    (each-segment-if
     (this 'absticks)
     (then
      (setq add 1000)
      (let ((diff (mod (this 'absticks) quant-incr))
            (diff3 (mod (this 'absticks) quant-incr3)))
        ;(print-ll "absticks " (this 'absticks) "  DIFF " diff "   DIFF3 " diff3)
        (if (< diff (abs add)) (setq add (- diff)))
        (if (< (- quant-incr diff) (abs add)) (setq add (- quant-incr diff)))
        (if (< diff3 (abs add)) (setq add (- diff3)))
        (if (< (- quant-incr3 diff3) (abs add)) (setq add (- quant-incr3 diff3))) )
      ;(print add)
      (add-this 'absticks add)
         ))))

(defun collect-notoffs-mark-ndrticks ()
  (each-segment-if
   (typep *this-segment* 'noteon)
   (> (velocity *this-segment*) 0)
   (not (last?))
   (then
    (let ((i (i?next-noteoff *i* (note-number *this-segment*))))
      (if (not i) (setq i (1+ *i*)))
      ;(set-this 'ndr (- (iget i 'abstime) (this 'abstime)))
      (set-this 'ndrticks (- (iget i 'absticks) (this 'absticks)))
      ;(print-ll "collect-notoffs-mark-dr: this ndr " (this 'ndr) " abstime " (this 'abstime))
      )))
  (each-segment-if
   (or (typep *this-segment* 'noteoff)
       (and (typep *this-segment* 'noteon)
            (= (velocity *this-segment*) 0) ))
   (then
    (remove-this-segment) )))

;utility for finding next noteoff (or noteon vel 0) with the given note-number                   
(defun i?next-noteoff (i note-number) 
  (untilexit end
             (incf i)
             (when (>= i *v-length*) 
               (warn "corresponding noteoff not found, note-number: ~A" note-number)
               (return-from end nil)
               )
             (if (or (and (typep (nth i *v*) 'noteoff)
                          (= note-number (note-number (nth i *v*))) )
                     (and (typep (nth i *v*) 'noteon)
                          (= (velocity (nth i *v*)) 0)
                          (= note-number (note-number (nth i *v*))) ))
                 (return-from end i)
               )))

;return the index of the next noteon
(defun i?next-noteon (i) 
   (untilexit end
     (incf i)
     (if (>= i *v-length*) (return-from end nil))
     (if (typep (nth i *v*) 'noteon)
        (return-from end i)
        )))

#|
(defun midifile-collect-programchange ()
   (each-segment-if
     (typep *this-segment* 'programChange)
     (then
       (let ((i (i?next-noteon *i*)))
          (if i (iset i 'pr (program *this-segment*)))
          (remove-this-segment)
         ))))
|#
;;chase program change forward
(defun midifile-collect-programchange ()
  (each-track
   (let ((track *this-track*) (found? nil))
     (each-segment-if
      (typep *this-segment* 'programChange)
      (then
       (cond ((not found?)  ;propagate first one to track var
              (setf (midi-initial-program track) (program *this-segment*))
              (setq found? t)
              (remove-this-segment) )
             (found?    ;keep the following in track
              (let ((i (i?next-noteon *i*)))
                (if i (iset i 'pr (program *this-segment*)))
                (remove-this-segment)
                ))))))))

;;chase bank change forward
(defun midifile-collect-bank-msb ()
  (each-track
   (let ((track *this-track*) (found? nil))
     (each-segment-if
      (typep *this-segment* 'midiBankMSB)
      (then
       (cond ((not found?)  ;propagate first one to track var
              (setf (midi-bank-msb track) (msb *this-segment*))
              (setq found? t)
              (remove-this-segment) )
             (found?    ;keep the following in track
              (let ((i (i?next-noteon *i*)))
                (if i (iset i 'bank-msb (msb *this-segment*)))
                (remove-this-segment)
                ))))))))
(defun midifile-collect-bank-lsb ()
  (each-track
   (let ((track *this-track*) (found? nil))
     (each-segment-if
      (typep *this-segment* 'midiBankLSB)
      (then
       (cond ((not found?)  ;propagate first one to track var
              (setf (midi-bank-lsb track) (lsb *this-segment*))
              (setq found? t)
              (remove-this-segment) )
             (found?    ;keep the following in track
              (let ((i (i?next-noteon *i*)))
                (if i (iset i 'bank-lsb (lsb *this-segment*)))
                (remove-this-segment)
                ))))))))

;;chase pan change forward
(defun midifile-collect-pan ()
  (each-track
   (let ((track *this-track*) (found? nil))
     (each-segment-if
      (typep *this-segment* 'midiPan)
      (then
       (cond ((not found?)  ;propagate first one to track var
              (setf (midi-pan track) (pan *this-segment*))
              (setq found? t)
              (remove-this-segment) )
             (found?    ;keep the following in track
              (let ((i (i?next-noteon *i*)))
                (if i (iset i 'pan (pan *this-segment*)))
                (remove-this-segment)
                ))))))))

;;chase reverb change forward
(defun midifile-collect-reverb ()
  (each-track
   (let ((track *this-track*) (found? nil))
     (each-segment-if
      (typep *this-segment* 'midiReverb)
      (then
       (cond ((not found?)  ;propagate first one to track var
              (setf (midi-reverb track) (reverb *this-segment*))
              (setq found? t)
              (remove-this-segment) )
             (found?    ;keep the following in track
              (let ((i (i?next-noteon *i*)))
                (if i (iset i 'reverb (reverb *this-segment*)))
                (remove-this-segment)
                ))))))))


;;;collect and removes pitch bend objects
;;; put them in a list on the previous note with delta-times
;;; final conversion to break point functions in midifile-mark-dr
#|
(defun midifile-collect-pitch-bend ()
  (let (note)
  (each-segment
   (if (typep *this-segment* 'noteon)
       (setq note *this-segment*) )
   (if (typep *this-segment* 'pitchBend)
       (then 
        (if (not (get-var note 'dc))
            (set-var note 'dc (list (cons (this 'absticks) (pitchbend *this-segment*))))
          (set-var note 'dc (append (get-var note 'dc) (list (cons (this 'absticks) (pitchbend *this-segment*))))) )
        (remove-this-segment)
        )))))
|#

;ignore pitchbend before first note (better to collect them but where?)
(defun midifile-collect-pitch-bend ()
  (let (note)
  (each-segment
   (if (typep *this-segment* 'noteon)
       (setq note *this-segment*) )
   (if (typep *this-segment* 'pitchBend)
       (then 
        (cond
         ((not note)) ;do nothing if before first note
         ((not (get-var note 'dc)) ;first pitchbend on that note
          (set-var note 'dc (list (cons (this 'absticks) (pitchbend *this-segment*)))) )
          (t  ;following pitchbends on same note
           (set-var note 'dc (append (get-var note 'dc) (list (cons (this 'absticks) (pitchbend *this-segment*))))) )
           )
        (remove-this-segment)
        )))))

;;take the first occurence of midivolume, move to track object and transform to db
(defun midifile-collect-midivolume ()
   (each-track
     (let ((track *this-track*))
        (each-segment-if
          (typep *this-segment* 'midiVolume)
          (then
            (setf (midi-initial-volume track) (midi-volume-to-vol-sblive (volume *this-segment*))) ;;back transformation according to sblive
            (exit-track)
           )))))

;;;(defun midifile-collect-sysex ()
;;;   (each-segment-if
;;;     (typep *this-segment* 'sysex-event)
;;;    (then
;;;     (let ((i (i?next-noteon *i*)))
;;;       (if i (iset i 'sysex (sysex *this-segment*)))
;;;       (remove-this-segment)
;;;       ))))

;move sysex segments to the list in score object
(defun midifile-collect-sysex ()
   (each-segment-if
     (typep *this-segment* 'sysex-event)
    (then
     (setf (sysex-list *active-score*) (append (sysex-list *active-score*) (sysex *this-segment*)))
     (remove-this-segment)
       )))

;move copyright notice segment to the list in score object
(defun midifile-collect-copyrightnotice ()
   (each-segment-if
     (typep *this-segment* 'copyrightNotice-event)
    (then
     (setf (copyrightnotice-string *active-score*) (append (copyrightnotice-string *active-score*) (copyright *this-segment*)))
     (remove-this-segment)
       )))

(defun midifile-set-channel ()
   (each-track
     (let ((track *this-track*))
        (each-segment-if
          (typep *this-segment* 'noteon)
          (then
            (setf (midi-channel track) (1+ (channel *this-segment*)))
            (set-this 'channel (1+ (channel *this-segment*)))
            (exit-track)
           )))))

;find the first occurence of settempo, timesignature and keysignature
;converts to DM format and put them in the first noteon of each track
(defun midifile-collect-tempo-meter-key ()
   (let ((mm (get-dm-var 'default-tempo))) 
      (block loop
        (each-segment-if
          (typep *this-segment* 'setTempo)
          (then
            (setq mm 
              (/ 60E6 (midi-tempo *this-segment*) ))
            (return-from loop) )))
      (each-segment-if
        (typep *this-segment* 'noteon)
        (then 
          (set-this 'mm mm)
          (exit-track)
          )))
   (let (meter) 
      (block loop
        (each-segment-if
          (typep *this-segment* 'timesignature)
          (then
            (setq meter 
              (list (nn *this-segment*) (expt 2 (dd *this-segment*)))
              )
            (return-from loop) )))
      (each-segment-if
        (typep *this-segment* 'noteon)
        (then 
          (set-this 'meter meter)
          (exit-track)
          )))
   (let (key modus) 
      (block loop
        (each-segment-if
          (typep *this-segment* 'keysignature)
          (then
            (setq key (midifile-timesignature-to-key
                       (sf *this-segment*)
                       (mi *this-segment*) ))
            (setq modus (midifile-timesignature-to-modus
                         (mi *this-segment*) ))
            (return-from loop) )))
      (if key
         (each-segment-if
           (typep *this-segment* 'noteon)
           (then 
             (set-this 'key key)
             (set-this 'modus modus)
             (exit-track)
             ))))
   )
   
;convert key from midifile format to Dm format
;fixed "negative values"
(defun midifile-timesignature-to-key (sf mi)
   ;(print-ll sf "  " mi)
   (cond 
         ((= mi 1) ;minor
          (cond 
                ((= sf 0) "A")((= sf #xFF) "D")
                ((= sf #xFE) "G")((= sf #xFD) "C")
                ((= sf #xFC) "F")((= sf #xFB) "Bb")
                ((= sf #xFA) "Eb")((= sf #xF9) "Ab") 
                ((= sf 1) "E")((= sf 2) "B")
                ((= sf 3) "F#")((= sf 4) "C#")
                ((= sf 5) "G#")((= sf 6) "D#")
                ((= sf 7) "A#")
                (t (warn (format nil "not valid key: ~A" sf))) ))
         ((= mi 0) ;major
          (cond
                ((= sf 0) "C") ((= sf #xFF) "F")
                ((= sf #xFE) "Bb") ((= sf #xFD) "Eb")
                ((= sf #xFC) "Ab") ((= sf #xFB) "Db")
                ((= sf #xFA) "Gb") ((= sf #xF9) "B") 
                ((= sf 1) "G") ((= sf 2) "D")
                ((= sf 3) "A") ((= sf 4) "E")
                ((= sf 5) "B") ((= sf 6) "F#")
                ((= sf 7) "C#")  
                (t (warn (format nil "not valid key: ~A" sf))) ))
    (t (warn (format nil "not valid modus: ~A" mi))) ))


(defun midifile-timesignature-to-modus (mi)
   (cond 
         ((= mi 1) ;minor
          "min")
         ((= mi 0) ;major
          "maj")
         (t (warn (format nil "not valid modus: ~A" mi)))
         ))



;;remove everything but notes and empty tracks
(defun midifile-cleanup ()
   (each-segment-if                            ;remove everything but notes
     (not (typep *this-segment* 'noteon))
     (then
       (remove-this-segment) ))
   (each-track                                    ;remove empty tracks such as the tempo track
     (if (eq nil (segment-list *this-track*))
        (remove-this-track)
        )))

;;;(defun midifile-init-var ()
;;;   (each-segment-if
;;;     (typep *this-segment* 'noteon)
;;;     (then
;;;       (set-this 'f0 (note-number *this-segment*))
;;;       (set-this 'sl 0)
;;;       ;(set-this 'dr (this 'ndr))
;;;      )))

;;added 'nsl parameter
(defun midifile-init-var ()
   (each-segment-if
     (typep *this-segment* 'noteon)
     (then
       (set-this 'f0 (note-number *this-segment*))
      (set-this 'nsl (vel-to-sl-sblive (velocity *this-segment*))) ;back transformation to dB according to sblive
      (set-this 'sl (this 'nsl))
      ;(set-this 'dr (this 'ndr))
       )))
          
(defun midifile-collect-chords ()
   (each-segment-if           ;collect chords
     (not (last?))
     (= (this 'absticks) (next 'absticks))
     (then
       (cond ((numberp (this 'f0))
              (set-next 'f0 (sort (list (this 'f0) (next 'f0)) #'>)) )
             ((listp (this 'f0))
              (set-next 'f0 (sort (append (this 'f0) (list (next 'f0))) #'>)) )
             (t (warn "f0 not a list or number:  ~A" (this 'f0)))
             )
       (if (this 'pr) (set-next 'pr (this 'pr)))
       (if (this 'channel) (set-next 'channel (this 'channel)))
       (if (this 'key) (set-next 'key (this 'key)))
       (if (this 'modus) (set-next 'modus (this 'modus)))
       (if (this 'mm) (set-next 'mm (this 'mm)))
       (if (this 'meter) (set-next 'meter (this 'meter)))
       (if (this 'sysex) (set-next 'sysex (this 'sysex)))
       (if (this 'dc) (set-next 'dc (this 'dc)))
       (if (this 'copyrightNotice) (set-next 'copyrightNotice (this 'copyrightNotice)))
      (remove-this-segment) )))


;;new version with articulation threshold
(defun midifile-insert-rest ()
   (each-segment-if
    (not (first?))
    (then
      (cond
       ((or (>= (+ (prev 'ndrticks)(prev 'absticks)) (this 'absticks)) ;cut dr at next onset if longer
            (< (/ (- (this 'absticks) (+ (prev 'ndrticks)(prev 'absticks))) ;disregard rest shorter than relative threshold
                  (- (this 'absticks) (prev 'absticks)))
               (get-dm-var 'midifile-input-articulation-threshold) ))
        (set-prev 'ndrticks (- (this 'absticks) (prev 'absticks)))
        )
      (t                                               ;add rest if longer than relative threshold
          (let ((segment (segment)))
             (set-var segment 'rest t)
             (set-var segment 'ndrticks (- (- (this 'absticks) (prev 'absticks))
                                      (prev 'ndrticks) ))
             (set-var segment 'absticks (+ (prev 'absticks) (prev 'ndrticks)))
            (insert-segment-before-this segment) )))))
  
   (each-track                ;insert rest before first note if absticks>0
     (each-segment
       (if (and (first?)
                (> (this 'absticks) 0) )
          (then
            (let ((segment (segment)))
              (set-var segment 'rest t)
              (set-var segment 'ndrticks (this 'absticks))
              (set-var segment 'absticks 0)
              (when (this 'key) (set-var segment 'key (this 'key)) (rem-this 'key))
              (when (this 'modus) (set-var segment 'modus (this 'modus)) (rem-this 'modus))
              (when (this 'mm) (set-var segment 'mm (this 'mm)) (rem-this 'mm))
              (when (this 'meter) (set-var segment 'meter (this 'meter)) (rem-this 'meter))
              (insert-segment-before-this segment) ))
          (exit-track) )))
  )

;;insert rest after last note
;;in progress
(defun midifile-insert-rest-last ()
  (let ((tot-ticks 0))
    (each-track                ;collect longest track duration
     (let ((last-segment (car (last (segment-list *this-track*)))))
       ;(print-ll "total dur in ticks : " (+ (get-var last-segment 'absticks)
       ;          (get-var last-segment 'ndrticks)))
       (if (> (+ (get-var last-segment 'absticks)
                 (get-var last-segment 'ndrticks))
              tot-ticks)
           (setq tot-ticks (+ (get-var last-segment 'absticks)
                              (get-var last-segment 'ndrticks)))
         )))
    (each-track                ;insert rest up to longest duration
     (let ((last-segment (car (last (segment-list *this-track*)))))
       (if (< (+ (get-var last-segment 'absticks)
                 (get-var last-segment 'ndrticks))
              tot-ticks)
            (let ((segment (segment)))
              (set-var segment 'rest t)
              (set-var segment 'ndrticks (- tot-ticks 
                                            (+ (get-var last-segment 'absticks)
                                               (get-var last-segment 'ndrticks)) ))
              (set-var segment 'absticks (+ (get-var last-segment 'absticks)
                                            (get-var last-segment 'ndrticks)) )
              (add-one-segment *this-track* segment)
              ))))
    ))

                                            
;;splits rests at barlines
(defun midifile-split-rest ()
  (let ((barticks (* 4 (division *active-score*)))) ;assumes 4/4 if no meter
   (each-segment-if
    (not (first?))
    (then
     (if (prev 'meter)                   ; ---collect ticks per measure---------------
         (setq barticks (* (car (prev 'meter)) 
                           (/ 4 (cadr (prev 'meter)))
                           (division *active-score*) )))
     (when (and (prev 'rest)             ;---process all rests but the last------------
                (cross-subdiv? (prev 'absticks) (prev 'ndrticks) barticks) )
       (let ((ticklist (subdiv-one-level (prev 'absticks) (prev 'ndrticks) barticks))
             (current-absticks (prev 'absticks)) )
         (set-prev 'ndrticks (car ticklist)) ;update the first rest
         (incf current-absticks (car ticklist))
         (dolist (tick (cdr ticklist))   ;generate the remaining rests
           (let ((segment (segment)))
             (set-var segment 'rest t)
             (set-var segment 'ndrticks tick)
             (set-var segment 'absticks current-absticks)
             (insert-segment-before-this segment) )
           (incf current-absticks tick) ))
       )
     (when (and (last?)                 ;---process last rest ---------------------------
                (this 'rest)
                (cross-subdiv? (this 'absticks) (this 'ndrticks) barticks) )
       (let ((ticklist (subdiv-one-level (this 'absticks) (this 'ndrticks) barticks))
             (current-absticks (this 'absticks)) )
         (set-this 'ndrticks (car ticklist)) ;update the first rest
         (incf current-absticks (car ticklist))
         (dolist (tick (cdr ticklist)) ;generate the remaining rests
           (let ((segment (segment)))
             (set-var segment 'rest t)
             (set-var segment 'ndrticks tick)
             (set-var segment 'absticks current-absticks)
             (add-one-segment *this-track* segment) )
           (incf current-absticks tick) ))
       (exit-track)
       )))))

;split notes at barlines
(defun midifile-split-note ()
  (let ((barticks (* 4 (division *active-score*)))) ;assumes 4/4 if no meter
   (each-segment-if
    (not (first?))
    (then
     (if (prev 'meter)                   ; ---collect ticks per measure---------------
         (setq barticks (* (car (prev 'meter)) 
                           (/ 4 (cadr (prev 'meter)))
                           (division *active-score*) )))
     (when (and (not (prev 'rest))             ;---process all notes but the last------------
                (cross-subdiv? (prev 'absticks) (prev 'ndrticks) barticks) )
       (let ((ticklist (subdiv-one-level (prev 'absticks) (prev 'ndrticks) barticks))
             (current-absticks (prev 'absticks)) )
         (set-prev 'ndrticks (car ticklist)) ;update the first note
         (set-prev 'tie t)
         (incf current-absticks (car ticklist))
         (dolist (tick (cdr ticklist))   ;generate the remaining notes
           (let ((segment (segment)))
             (set-var segment 'f0 (prev 'f0))
             (set-var segment 'sl (prev 'sl))
             (set-var segment 'ndrticks tick)
             (set-var segment 'absticks current-absticks)
             (insert-segment-before-this segment) )
           (incf current-absticks tick) ))
       )
     (when (and (last?)                 ;---process last rest ---------------------------
                (not (this 'rest))
                (cross-subdiv? (this 'absticks) (this 'ndrticks) barticks) )
       (let ((ticklist (subdiv-one-level (this 'absticks) (this 'ndrticks) barticks))
             (current-absticks (this 'absticks)) )
         (set-this 'ndrticks (car ticklist)) ;update the first note
         (set-this 'tie t)
         (incf current-absticks (car ticklist))
         (dolist (tick (cdr ticklist)) ;generate the remaining notes
           (let ((segment (segment)))
             (set-var segment 'f0 (this 'f0))
             (set-var segment 'sl (this 'sl))
             (set-var segment 'ndrticks tick)
             (set-var segment 'absticks current-absticks)
             (add-one-segment *this-track* segment) )
           (incf current-absticks tick) ))
       (exit-track)
       )))))


(defun start-on-subdiv? (absticks subdiv)
  (zerop (mod absticks subdiv)) )

(defun cross-subdiv? (absticks ndrticks subdiv)
  (> (+ ndrticks (mod absticks subdiv)) subdiv) )

(defun ticks-to-next (absticks subdiv)
  (- subdiv (mod absticks subdiv)) )

;;subdivide a segment tick duration into a list of ndrticks
;;considering the subdiv boundaries
(defun subdiv-one-level (absticks ndrticks subdiv)
  (cond
   ;---no crossing-----------------------------
   ((not (cross-subdiv? absticks ndrticks subdiv))
    (list ndrticks) )
   ;--- crossing and start on the subdiv-------
   ((and (start-on-subdiv? absticks subdiv)
         (cross-subdiv? absticks ndrticks subdiv) )
    (append (list subdiv)
            (subdiv-one-level (+ absticks subdiv) (- ndrticks subdiv) subdiv)
            ))
   ;---crossing and start between subdiv--------
   ((and (not (start-on-subdiv? absticks subdiv))
         (cross-subdiv? absticks ndrticks subdiv) )
    (append (list (ticks-to-next absticks subdiv))
            (subdiv-one-level (+ absticks (ticks-to-next absticks subdiv))
                              (- ndrticks (ticks-to-next absticks subdiv))
                              subdiv )
            ))
   ))

#|
(defun midifile-mark-dr ()
  (let (tempo-factor) 
    (each-segment
     (if (this 'mm) (setq tempo-factor 
                          (/ (/ (/ 60E6 (this 'mm)) 1000.0)
                             (division *active-score*))))
     (set-this 'ndr (* tempo-factor (this 'ndrticks)))
     (set-this 'dr (this 'ndr))
     )))
|#
;;with duration limit 1ms
#|
(defun midifile-mark-dr ()
  (let (tempo-factor ioi) 
    (each-segment
     (if (this 'mm) (setq tempo-factor 
                          (/ (/ (/ 60E6 (this 'mm)) 1000.0)
                             (division *active-score*))))
     (when (zerop (this 'ndrticks))  ;minimum 1 ms ioi limit
       (set-this 'ndrticks 1)
       (if (not (last?)) (add-next 'ndrticks -1)) )
     (setq ioi (* tempo-factor (this 'ndrticks)))
     (set-this 'ndr ioi)
     (set-this 'dr ioi)
     )))
|#
;;with conversion of pitch bend bp
(defun midifile-mark-dr ()
  (let (tempo-factor ioi) 
    (each-segment
     (if (this 'mm) (setq tempo-factor 
                          (/ (/ (/ 60E6 (this 'mm)) 1000.0)
                             (division *active-score*))))
     (when (zerop (this 'ndrticks))  ;minimum 1 ms ioi limit
       (set-this 'ndrticks 1)
       (if (not (last?)) (add-next 'ndrticks -1)) )
     (setq ioi (* tempo-factor (this 'ndrticks)))
     (set-this 'ndr ioi)
     (set-this 'dr ioi)
     (when (this 'dc)
       (let ((tickstart (this 'absticks)))
         (set-this 'dc
                   (this-segment-make-time-shape 
                    :interpolation :staircase
                    :y-max 200
                    :y-min -200
                    :break-point-list
                    (alist-to-list (mapcar #'(lambda (x) (cons (* tempo-factor (- (car x) tickstart))
                                                               (round (* 200 (/ (cdr x) 8192))) ))
                                     (this 'dc) )))
                   ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;;------------ guess notevalues from quantized values------------------------
;;this algorithm assumes a strict metric subdivision
;;the subdivision of note values are mostly according to current practice 
;;6/8 is missing right now
;;ticks will be subdivided until they are smaller that a 1/64
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;special cases of subdivision:
;;6/8 -> 3/8 1/4 ...
;;4/4 or 2/2 or 8/4 or 4/2 -> 1/2 ...
;; all the rest starts with 1/4:
;;2/4 -> 1/4 ...
;;4/8 -> 1/4 ...
;;3/4 -> 1/4 ...
;;3/8 -> 1/8 ...

;;an "internal" variabel that count the number of
;;times the midifile-get-notevalue-quant function is applied
(defvar *midifile-note-subdiv-level*)

(defun midifile-guess-notevalues-quant ()
  (let ((meter '(4 4))
        (division (division *active-score*))
        notev )
    (each-segment
     (setq *midifile-note-subdiv-level* 0) ;reset counter
     (if (this 'meter) (setq meter (this 'meter)))
     (cond
      ;;1/2 subdivision
      ((or (equal meter '(4 4))(equal meter '(2 2))(equal meter '(8 4))(equal meter '(4 2)))
       (setq notev (midifile-get-notevalue-quant 
                    (this 'absticks) (this 'ndrticks) (* division 4) )))
      ;;otherwise 1/4 note subdivision
      (t 
       (setq notev (midifile-get-notevalue-quant 
                    (this 'absticks) (this 'ndrticks) (* division 2) )))
      )
     (print notev)
     (if (> (length notev) 1) (setq notev (list (apply #'+ notev)))) ;add all fractions to one -adaptation to clj-dm (120607/af)
     (set-this 'n (cons 
                   (if (this 'rest) '() (f0-to-toneoctave (this 'f0)))
                   notev ))
     )))


;;find right note value or subdivide further
;faster with member than case
(defun midifile-get-notevalue-quant (absticks ndrticks subdiv)
  (incf *midifile-note-subdiv-level*)
  (let ((ratio (/ (/ ndrticks (division *active-score*)) 4)));4 since division is quarter note
    (cond
     ((> *midifile-note-subdiv-level* (get-dm-var 'midifile-note-subdiv-levels))
      (list ratio) )
     ((< ratio 1/64)
      (list ratio) )
     ((member ratio 
              '(1 1/2 1/4 1/8 1/16 1/32 1/64 ;normal
                3/2 3/4 3/8 3/16 3/32 3/64     ;dotted
                1/3 1/6 1/12 1/24 1/48 1/64))  ;triplet
      (list ratio) )
     (t
      (let ((list '())
            (abst absticks))
        (dolist (ndrt (subdiv-one-level absticks ndrticks subdiv))
          (setq list (append list (midifile-get-notevalue-quant abst ndrt (/ subdiv 2)) ))
          (incf abst ndrt) )
        list))
     )))

#| not used
;;searches the exact note value or return nil
(defun midifile-get-one-notevalue-quant (ndrticks)
  (let ((division (division *active-score*)))
    (case (/ (/ ndrticks division) 4) ;4 since division is quarter note
      (1 1)        ;normal
      (1/2 1/2)
      (1/4 1/4)
      (1/8 1/8)
      (1/16 1/16)
      (1/32 1/32)
      (1/64 1/64)
      (3/2 3/2)     ;dotted
      (3/4 3/4)
      (3/8 3/8)
      (3/16 3/16)
      (3/32 3/32)
      (3/64 3/64)
      (1 1)        ;triplet
      (1/3 1/3)
      (1/6 1/6)
      (1/12 1/12)
      (1/24 1/24)
      (1/48 1/48)
      (1/64 1/64)
      (t nil)      ;not found - return nil
      )))
|#


#|  this is hopefully obsolete with the new one above
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       
;;------------ guess notevalues ---------------------------------------------
;; this is the "free" note value version
;; intended to be used when a performed midi file is read
;; the guessed note value may consist of some strange values
;; notevalues will be selected from midifile-input-notevalues given in dm-objects
;; the nearest corresponding duration will be chosen
;; no quantization of durations is performed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(1/8 1/16 3/16 1/12 1/6)

#|
(defvar *midifile-notevalues*)
(setq *midifile-notevalues* 
  (sort (list
      1/32 1/16 3/32 1/8 5/32 3/16 7/32 ;32nd division
      1/24 1/12 3/24 1/6 5/24            ;triplets on a eighth note
         )  #'>))

;;0009/af also 1/64
(setq *midifile-notevalues*
  (sort (list
      1/64 1/32 1/16 3/32 1/8 5/32 3/16 7/32 ;64nd division
      1/24 1/12 1/6 5/24            ;triplets on a eighth note
         )  #'>))  
|#

(defun midifile-guess-notevalues ()
  (let ((notevalue-table 
         (midifile-convert-notevalue-list (get-dm-var 'midifile-input-notevalues)) ))
        (each-segment
   (if (not (this 'rest))
   (set-this 'n (cons 
                 (f0-to-toneoctave (this 'f0))
                 (midifile-select-note (this 'ndrticks) notevalue-table) )))
   (if (this 'rest)
   (set-this 'n (cons 
                 '()
                 (midifile-select-note (this 'ndrticks) notevalue-table) )))
   )))

       
        
 ;preprocessing of notevalue-list
 ;gives tickdurations in the middle between notevalues
(defun midifile-convert-notevalue-list (notevalue-list)
   (let ((table nil)
         (division (division *active-score*))
         (outtable nil) )
      (dolist (value notevalue-list)
         (setq table (append table (list (list (round (* value 4 division)) value )))))
      ;(print table)
      (loop for i from 0 to (- (length table) 2) do          ;set the rest
        (setq outtable (append outtable 
          (list (list (round (+ (car (nth i table))
                        (/ (- (car (nth (1+ i) table))
                              (car (nth i table)))
                        2.0) ))
            (cadr (nth i table)) )))))
      (setq outtable (append outtable                   ;set the last
            (list (list (round (/ (caar (last table)) 2.0)) (cadar (last table)))) ))
      outtable))

(defun midifile-select-note (ndrticks table)
   (let ((division (division *active-score*))
         (minval (caar (last table))) )
      (cond 
            ((> ndrticks (- (* 4 division) minval)) ;whole note
             (append (list 1) (midifile-select-note (- ndrticks (* 4 division)) table)) )
            ((> ndrticks (- (* 2 division) minval)) ;half note
             (append (list 1/2) (midifile-select-note (- ndrticks (* 2 division)) table)) )
            ((> ndrticks (- division minval))       ;quarter note
             (append (list 1/4) (midifile-select-note (- ndrticks division) table)) )
            (t
             (dolist (val table)
               (when (> ndrticks (car val))
               (return (list (cadr val)))
                 ))))))

#| 
(defun midifile-select-note (ndrticks table)
   (let ((division (division *active-score*))
         (minval (caar (last table))) )
             (dolist (val table)
               (when (> ndrticks (car val))
                 (return (append (list (cadr val))
                                 (midifile-select-note (- ndrticks (* 4  (cadr val) division)) table) )))
               (when (< ndrticks minval)
                 (return nil) )               
               )))
|#
;new version with just a list of all possible notevalues
(defun midifile-select-note (ndrticks table)
  (if (< ndrticks (caar (last table))) ;if less than minval return nil
      nil 
    (let ((division (division *active-score*)))
      (dolist (val table)
        (when (> ndrticks (car val))
          (return (append (list (cadr val))
                          (midifile-select-note (- ndrticks (* 4  (cadr val) division)) table) )))
        ))))

#| moved to dm-objects
(set-dm-var 'midifile-input-notevalues
            (sort (list
                   1/64 1/32 1/16 1/8 1/4 1/2 1
                   (* 1/3 1) (* 2/3 1)     ;half note triplets
                   (* 1/3 1/2) ;(* 2/3 1/2)     ;quarter note triplets
                   (* 1/3 1/4) ;(* 2/3 1/4)     ;eighth note triplets
                   (* 1/3 1/8) ;(* 2/3 1/8)     ;sixteens note triplets
                   (* 1/3 1/16) ;(* 2/3 1/16)     ;thirtysecond note triplets
                   (* 3/2 1/2)(* 3/2 1/4)(* 3/2 1/8)(* 3/2 1/16)(* 3/2 1/32)
                   )  #'>))
|#
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  ----- utility functions -----
;;

;;
;;  translateVariableLengthQuantity
;;
;;  Translates a variable length quantity from the beginning of a stream
;;

(defun translate-variable-length-quantity (f)
  (declare (stream f))
  (let (b (sum 0))
    (declare (integer b sum))
    (loop while (logbitp 7 (setq b (read-byte f nil nil))) do
          (setq sum (ash (+ sum (logand b 127)) 7)))
    (+ sum b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  peek-octet
;;
;;  Using read-octet instead of read-byte on a "ccl::input-binary-file-stream" adds the feature
;;  to look at the first element of the stream without reading it.
;;  If a *midifile-buffer* is non nil, the peek-octet reads one byte using read-byte and puts it in *midifile-buffer*,
;;  returning the read byte. If a buffer exists, peek-octet returns the value contained in *midifile-buffer*.
;;  

(defmethod peek-octet (istream)
  (if *midifile-buffer*
    *midifile-buffer*
    (setq *midifile-buffer* (read-byte istream nil nil))))

;;; (defmethod peek-octet ((istream ccl::input-binary-file-stream))
;;;   (if *midifile-buffer*
;;;     *midifile-buffer*
;;;     (setq *midifile-buffer* (read-byte istream nil nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  read-octet
;;
;;  Using read-octet instead of read-byte on a "ccl::input-binary-file-stream" adds the feature
;;  to look at the first element of the stream without reading it.
;;  If the previous stream operation was a peek-octet, then read-octet gets its value from *midifile-buffer*,
;;  otherwise it uses the function read-byte.
;;  Make sure the global variable *midifile-buffer* is nil before starting using read-octet and peek-octet.
;;

(defmethod read-octet (istream &optional eof-errorp eof-value)
  (if *midifile-buffer*
    (prog1
      *midifile-buffer*
      (setq *midifile-buffer* nil))
    (read-byte istream eof-errorp eof-value)))

;;; (defmethod read-octet ((istream ccl::input-binary-file-stream) &optional eof-errorp eof-value)
;;;   (if *midifile-buffer*
;;;     (prog1
;;;       *midifile-buffer*
;;;       (setq *midifile-buffer* nil))
;;;     (read-byte istream eof-errorp eof-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  read-string
;;  reads n numbers of characters and converts it to a string

;;;(defmethod read-string (istream n &optional eof-errorp eof-value)
;;;  (let ((utstring ""))
;;;    (dotimes (i n)
;;;      (setq utstring 
;;;            (concatenate 'string
;;;                         utstring
;;;                         (string (character (read-byte istream eof-errorp eof-value))))) )
;;;    ;(print utstring)
;;;    utstring ))

;;2004-04-26/af changed to "code-char" works better with linux
(defmethod read-string (istream n &optional eof-errorp eof-value)
  (let ((utstring ""))
    (dotimes (i n)
      (setq utstring 
            (concatenate 'string
                         utstring
                         (string (code-char (read-byte istream eof-errorp eof-value))))) )
    ;(print utstring)
    utstring ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  read-MTrk
;;
;;  Translates to midi-messages from a stream, sets track to a list of objects of type MTrk-event.
;;  Translates until finding an End of Track Meta-event don't bothering about the track size.
;;


;;ticks for not recognized events are added up!!!!!!!!!!!!!!!!!!!!!
#|
(defun read-MTrk (istream track)
   (declare (stream istream))
   ;(print track)
   (let ((delta-time 0) (midi-list '()) (ready nil) (status 0) (x 0))
  (declare (integer delta-time status temp x) (list midi-list))
     (setq *midifile-buffer* nil)                                                 ;; reset input buffer
     (setq delta-time (translate-variable-length-quantity istream)) ;read first deltatime
     (loop until ready do
          (when (logbitp 7 (peek-octet istream))              ;; new status - not running status
;            (format t "~%x ~D new status ~D" x status)
            (setq x (read-octet istream nil nil)
                  status (ash x -4)))
          (case status
            
            ;-------NOTE OFF---------
            (#x8 (add-one-segment track (make-instance 'noteOff
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                      :note-number (read-octet istream nil nil)
                                      :velocity (read-octet istream nil nil) ))
             (setq delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~D noteOff" delta-time)) )
            
            ;-------NOTE ON---------
            (#x9 (add-one-segment track (make-instance 'noteOn
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                      :note-number (read-octet istream nil nil)
                                      :velocity (read-octet istream nil nil)) )   ;; if 0 the noteOff
                 (setq delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~%~D noteOn " delta-time)))
            
            ;-------POLYPHONIC KEY PRESSURE/AFTERTOUCH---------
            (#xA (when *mf-debug-info* (format t "~%not implemented Polyphonic key pressure/Aftertouch!"))
             (loop repeat 2 do (read-octet istream nil nil))
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------SELECT CHANNEL MODE OR CONTROL CHANGE---------
            (#xB (when *mf-debug-info* (format t "~%not implemented Select Channel Mode or Control Change!") )
             (loop repeat 2 do (read-octet istream nil nil))
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------PROGRAM CHANGE---------
            (#xC (add-one-segment track (make-instance 'programChange
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                          :program (1+ (read-octet istream nil nil)) ))
             (setq delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~%~D programChange " delta-time)) )
            
            ;-------CHANNEL PRESSURE---------
            (#xD (when *mf-debug-info* (format t "~%not implemented Channel Pressure!") )
             (read-octet istream nil nil)
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------PITCH BEND---------
            (#xE (when *mf-debug-info* (format t "~%not implemented Pitch bend change!") )
             (loop repeat 2 do (read-octet istream nil nil))
             (incf delta-time (translate-variable-length-quantity istream)))
            
            ;-------SYSTEM EXCLUSIVE AND META EVENTS----------
            (#xF (case x
                   (#xF0 
                    (let ((length (translate-variable-length-quantity istream)) 
                          (l '()))
                      (loop repeat length do (newr l (read-octet istream nil nil)))
                      ;(add-one-segment track (make-instance 'sysex-event :delta-time delta-time :sysex l ))
                      (setf (sysex-list *active-score*) (append (sysex-list *active-score*) (list l)))  ;write direct in score object all sysex in a list of lists             
                      (when *mf-debug-info* (format t "~%~D system exclusive " delta-time)) )
                    (incf delta-time (translate-variable-length-quantity istream)) )
                   (#xF7 (when *mf-debug-info* (format t "~%not implemented System Exclusive \"escape\"!"))
                         (let ((length (translate-variable-length-quantity istream)))
                           (loop repeat length do (read-octet istream nil nil)))
                         (incf delta-time (translate-variable-length-quantity istream)) )
            
                   ;-------META EVENTS---------
                   (#xFF (let ((meta-type (read-octet istream nil nil))
                               (meta-length (translate-variable-length-quantity istream)))
                           (declare (integer meta-length meta-type))
                           (when *mf-debug-info*
                             (format t "~%META-EVENT type: ~X length: ~D" meta-type meta-length))
                           (case meta-type
                             (#x00 (when *mf-debug-info* (format t "~%not implemented Sequence Number!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )

			     
			     ;-------track name---------
                             (#x03 (when *mf-debug-info* (format t "~%Track Name Meta Event"))
                              (setf (trackname track) (read-string istream meta-length))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             (#x20 (when *mf-debug-info* (format t "~%not implemented MIDI Channel Prefix!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)))
                             ;-------end of track---------
                             (#x2F (when *mf-debug-info* (format t "~%endOfTrack"))
                                   (add-one-segment track (make-instance 'endOfTrack
                                                        :delta-time delta-time) )
                                   (setq ready t)
                              (loop repeat meta-length do (read-octet istream nil nil)))
                             ;-------set tempo---------
                             (#x51 (when *mf-debug-info* (format t "~%setTempo"))
                              (add-one-segment track
                                    (make-instance 'setTempo
                                                   :delta-time delta-time
                                                   :midi-tempo  (+ (ash (read-octet istream nil nil) 16)
                                                                    (ash (read-octet istream nil nil) 8)
                                                                    (read-octet istream nil nil))) )
                              (loop repeat (- meta-length 3) do (read-octet istream nil nil))
                              (setq delta-time (translate-variable-length-quantity istream)) )
                             (#x54 (when *mf-debug-info* (format t "~%not implemented SMPTE Offset!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             ;-------time signature---------
                             (#x58 (add-one-segment track (make-instance 'timeSignature
                                                        :delta-time delta-time
                                                        :nn (read-octet istream nil nil)
                                                        :dd (read-octet istream nil nil)
                                                        :cc (read-octet istream nil nil)
                                                        :bb (read-octet istream nil nil))
                                         )
                              (loop repeat (- meta-length 4) do (read-octet istream nil nil))
                              (setq delta-time (translate-variable-length-quantity istream)) )
                             ;-------key signature---------
                             (#x59 (add-one-segment track (make-instance 'keySignature
                                                        :delta-time delta-time
                                                        :sf (read-octet istream nil nil)
                                                        :mi (read-octet istream nil nil))
                                         )
                              (loop repeat (- meta-length 2) do (read-octet istream nil nil))
                              (setq delta-time (translate-variable-length-quantity istream)) )
                             ;-------other meta events---------
                             (t    (cond                                 ;; other Meta-Events
                                    ((and (>= meta-type #x01)
                                          (<= meta-type #x0F))
                                     (when *mf-debug-info* (format t "~%not implemented Text Event of type ~D!" meta-type)))
                                    ((= meta-type #x7F)
                                     (when *mf-debug-info* (format t "~%not implemented Sequencer-Specific Meta-Event!")))
                                    (t
                                     (when *mf-debug-info* 
                                         (format t "~%not implemented Meta-Event of type ~D!" meta-type))))
                                (loop repeat meta-length do (read-octet istream nil nil))
                                (incf delta-time (translate-variable-length-quantity istream)) ))))
                   (t    (error "wrong type of MTrk event"))))
            (t    (error "wrong type of MTrk event"))))
    ))

;new version with type 0 split
(defun read-MTrk (istream track)
   (declare (stream istream))
   ;(print track)
   (let ((delta-time 0) (midi-list '()) (ready nil) (status 0) (x 0))
  (declare (integer delta-time status x) (list midi-list))
     (setq *midifile-buffer* nil)                                                 ;; reset input buffer
     (setq delta-time (translate-variable-length-quantity istream)) ;read first deltatime
     (loop until ready do
          (when (logbitp 7 (peek-octet istream))              ;; new status - not running status
;            (format t "~%x ~D new status ~D" x status)
            (setq x (read-octet istream nil nil)
                  status (ash x -4)))
          (case status
            
            ;-------NOTE OFF---------
            (#x8 (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'noteOff
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                      :note-number (read-octet istream nil nil)
                                      :velocity (read-octet istream nil nil) ))
             (incf delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~D noteOff" delta-time)) )
            
            ;-------NOTE ON---------
            (#x9 (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'noteOn
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                      :note-number (read-octet istream nil nil)
                                      :velocity (read-octet istream nil nil)) )   ;; if 0 the noteOff
                 (incf delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~%~D noteOn " delta-time)))
            
            ;-------POLYPHONIC KEY PRESSURE/AFTERTOUCH---------
            (#xA (when *mf-debug-info* (format t "~%not implemented Polyphonic key pressure/Aftertouch!"))
             (loop repeat 2 do (read-octet istream nil nil))
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------SELECT CHANNEL MODE OR CONTROL CHANGE---------
            (#xB (when *mf-debug-info* (format t "~%not implemented Select Channel Mode or Control Change!") )
             (loop repeat 2 do (read-octet istream nil nil))
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------PROGRAM CHANGE---------
            (#xC (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'programChange
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                          :program (1+ (read-octet istream nil nil)) ))
             (incf delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~%~D programChange " delta-time)) )
            
            ;-------CHANNEL PRESSURE---------
            (#xD (when *mf-debug-info* (format t "~%not implemented Channel Pressure!") )
             (read-octet istream nil nil)
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------PITCH BEND---------
            (#xE (when *mf-debug-info* (format t "~%not implemented Pitch bend change!") )
             (loop repeat 2 do (read-octet istream nil nil))
             (incf delta-time (translate-variable-length-quantity istream)))
            
            ;-------SYSTEM EXCLUSIVE AND META EVENTS----------
            (#xF (case x
                   (#xF0 
                    (let ((length (translate-variable-length-quantity istream)) 
                          (l '()))
                      (loop repeat length do (newr l (read-octet istream nil nil)))
                      ;(add-one-segment track (make-instance 'sysex-event :delta-time delta-time :sysex l ))
                      (setf (sysex-list *active-score*) (append (sysex-list *active-score*) (list l)))  ;write direct in score object all sysex in a list of lists             
                      (when *mf-debug-info* (format t "~%~D system exclusive " delta-time)) )
                    (incf delta-time (translate-variable-length-quantity istream)) )
                   (#xF7 (when *mf-debug-info* (format t "~%not implemented System Exclusive \"escape\"!"))
                         (let ((length (translate-variable-length-quantity istream)))
                           (loop repeat length do (read-octet istream nil nil)))
                         (incf delta-time (translate-variable-length-quantity istream)) )
            
                   ;-------META EVENTS---------;for type 0: set all meta in tempo track

                   (#xFF (let ((meta-type (read-octet istream nil nil))
                               (meta-length (translate-variable-length-quantity istream)))
                           (declare (integer meta-length meta-type))
                           (when *mf-debug-info*
                             (format t "~%META-EVENT type: ~X length: ~D" meta-type meta-length))
                           (case meta-type
                             (#x00 (when *mf-debug-info* (format t "~%not implemented Sequence Number!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )

			     ;-------copyright notice---------
                             (#x02 (when *mf-debug-info* (format t "~%copyrightNotice Meta Event"))
                                   (setf (copyrightnotice-string *active-score*)  (read-string istream meta-length))
				   (incf delta-time (translate-variable-length-quantity istream)))

			     ;-------track name---------
                             (#x03 (when *mf-debug-info* (format t "~%Track Name Meta Event"))
                              (setf (trackname (if track track (nth 16 (track-list *active-score*))))
                                               (read-string istream meta-length) )
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             (#x20 (when *mf-debug-info* (format t "~%not implemented MIDI Channel Prefix!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)))
                             ;-------end of track---------
                             (#x2F (when *mf-debug-info* (format t "~%endOfTrack"))
                              (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                               (make-instance 'endOfTrack :delta-time delta-time) )
                                   (setq ready t)
                              (loop repeat meta-length do (read-octet istream nil nil)))
                             ;-------set tempo---------
                             (#x51 (when *mf-debug-info* (format t "~%setTempo"))
                              (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                    (make-instance 'setTempo
                                                   :delta-time delta-time
                                                   :midi-tempo  (+ (ash (read-octet istream nil nil) 16)
                                                                    (ash (read-octet istream nil nil) 8)
                                                                    (read-octet istream nil nil))) )
                              (loop repeat (- meta-length 3) do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             (#x54 (when *mf-debug-info* (format t "~%not implemented SMPTE Offset!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             ;-------time signature---------
                             (#x58 (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                                    (make-instance 'timeSignature
                                                        :delta-time delta-time
                                                        :nn (read-octet istream nil nil)
                                                        :dd (read-octet istream nil nil)
                                                        :cc (read-octet istream nil nil)
                                                        :bb (read-octet istream nil nil))
                                         )
                              (loop repeat (- meta-length 4) do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             ;-------key signature---------
                             (#x59 (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                                    (make-instance 'keySignature
                                                        :delta-time delta-time
                                                        :sf (read-octet istream nil nil)
                                                        :mi (read-octet istream nil nil))
                                         )
                              (loop repeat (- meta-length 2) do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             ;-------other meta events---------
                             (t    (cond                                 ;; other Meta-Events
                                    ((and (>= meta-type #x01)
                                          (<= meta-type #x0F))
                                     (when *mf-debug-info* (format t "~%not implemented Text Event of type ~D!" meta-type)))
                                    ((= meta-type #x7F)
                                     (when *mf-debug-info* (format t "~%not implemented Sequencer-Specific Meta-Event!")))
                                    (t
                                     (when *mf-debug-info* 
                                         (format t "~%not implemented Meta-Event of type ~D!" meta-type))))
                                (loop repeat meta-length do (read-octet istream nil nil))
                                (incf delta-time (translate-variable-length-quantity istream)) ))))
                   (t    (error "wrong type of MTrk event"))))
            (t    (error "wrong type of MTrk event"))))
     ))
|#

;;added input midi volume
;;added pich bend read
(defun read-MTrk (istream track)
   (declare (stream istream))
   ;(print track)
   (let ((delta-time 0) (midi-list '()) (ready nil) (status 0) (x 0))
  (declare (integer delta-time status x) (list midi-list))
     (setq *midifile-buffer* nil)                                                 ;; reset input buffer
     (setq delta-time (translate-variable-length-quantity istream)) ;read first deltatime
     (loop until ready do
          (when (logbitp 7 (peek-octet istream))              ;; new status - not running status
;            (format t "~%x ~D new status ~D" x status)
            (setq x (read-octet istream nil nil)
                  status (ash x -4)))
          (case status
            
            ;-------NOTE OFF---------
            (#x8 (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'noteOff
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                      :note-number (read-octet istream nil nil)
                                      :velocity (read-octet istream nil nil) ))
             (incf delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~D noteOff" delta-time)) )
            
            ;-------NOTE ON---------
            (#x9 (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'noteOn
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                      :note-number (read-octet istream nil nil)
                                      :velocity (read-octet istream nil nil)) )   ;; if 0 the noteOff
                 (incf delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~%~D noteOn " delta-time)))
            
            ;-------POLYPHONIC KEY PRESSURE/AFTERTOUCH---------
            (#xA (when *mf-debug-info* (format t "~%not implemented Polyphonic key pressure/Aftertouch!"))
             (loop repeat 2 do (read-octet istream nil nil))
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------SELECT CHANNEL MODE OR CONTROL CHANGE---------
;;;            (#xB (when *mf-debug-info* (format t "~%not implemented Select Channel Mode or Control Change!") )
;;;             (loop repeat 2 do (read-octet istream nil nil))
;;;             (incf delta-time (translate-variable-length-quantity istream)) )
            (#xB (case (read-octet istream nil nil) ;;read control number
                   (7                                ;volume
                    (when *mf-debug-info* (format t "~%~D midiVolume " delta-time))
                    (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'midiVolume
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                    :volume (read-octet istream nil nil) )))
                   (0                                ;bank MSB
                    (when *mf-debug-info* (format t "~%~D midiBankMSB " delta-time))
                    (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'midiBankMSB
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                    :msb (read-octet istream nil nil) )))
                   (32                                ;bank LSB
                    (when *mf-debug-info* (format t "~%~D midiBankLSB " delta-time))
                    (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'midiBankLSB
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                    :lsb (read-octet istream nil nil) )))
                   (10                                ;Pan
                    (when *mf-debug-info* (format t "~%~D midiPan " delta-time))
                    (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'midiPan
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                    :pan (read-octet istream nil nil) )))
                   (91                                ;Reverb
                    (when *mf-debug-info* (format t "~%~D midiReverb " delta-time))
                    (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'midiReverb
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                    :reverb (read-octet istream nil nil) )))
                   (t                
                    (when *mf-debug-info* (format t "~%not implemented Select Channel Mode or Control Change!"))
                    (read-octet istream nil nil) ))
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------PROGRAM CHANGE---------
            (#xC (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'programChange
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                          :program (1+ (read-octet istream nil nil)) ))
             (incf delta-time (translate-variable-length-quantity istream))
             (when *mf-debug-info* (format t "~%~D programChange " delta-time)) )
            
            ;-------CHANNEL PRESSURE---------
            (#xD (when *mf-debug-info* (format t "~%not implemented Channel Pressure!") )
             (read-octet istream nil nil)
             (incf delta-time (translate-variable-length-quantity istream)) )
            
            ;-------PITCH BEND---------
;;;            (#xE (when *mf-debug-info* (format t "~%not implemented Pitch bend change!") )
;;;             (loop repeat 2 do (read-octet istream nil nil))
;;;             (incf delta-time (translate-variable-length-quantity istream)))
            (#xE 
             (add-one-segment (if track track (nth (logand x 15) (track-list *active-score*)))
                                  (make-instance 'pitchBend
                                      :delta-time delta-time
                                      :channel (logand x 15)
                                          :pitchbend (- (+ (read-octet istream nil nil) (* 128 (read-octet istream nil nil))) 8192) ))
             (incf delta-time (translate-variable-length-quantity istream)))
            
            ;-------SYSTEM EXCLUSIVE AND META EVENTS----------
            (#xF (case x
                   (#xF0 
                    (let ((length (translate-variable-length-quantity istream)) 
                          (l '()))
                      (loop repeat length do (newr l (read-octet istream nil nil)))
                      ;(add-one-segment track (make-instance 'sysex-event :delta-time delta-time :sysex l ))
                      (setf (sysex-list *active-score*) (append (sysex-list *active-score*) (list l)))  ;write direct in score object all sysex in a list of lists             
                      (when *mf-debug-info* (format t "~%~D system exclusive " delta-time)) )
                    (incf delta-time (translate-variable-length-quantity istream)) )
                   (#xF7 (when *mf-debug-info* (format t "~%not implemented System Exclusive \"escape\"!"))
                         (let ((length (translate-variable-length-quantity istream)))
                           (loop repeat length do (read-octet istream nil nil)))
                         (incf delta-time (translate-variable-length-quantity istream)) )
            
                   ;-------META EVENTS---------;for type 0: set all meta in tempo track

                   (#xFF (let ((meta-type (read-octet istream nil nil))
                               (meta-length (translate-variable-length-quantity istream)))
                           (declare (integer meta-length meta-type))
                           (when *mf-debug-info*
                             (format t "~%META-EVENT type: ~X length: ~D" meta-type meta-length))
                           (case meta-type
                             (#x00 (when *mf-debug-info* (format t "~%not implemented Sequence Number!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )

			     ;-------copyright notice---------
                             (#x02 (when *mf-debug-info* (format t "~%copyrightNotice Meta Event"))
                                   (setf (copyrightnotice-string *active-score*)  (read-string istream meta-length))
				   (incf delta-time (translate-variable-length-quantity istream)))

			     ;-------track name---------
                             (#x03 (when *mf-debug-info* (format t "~%Track Name Meta Event"))
                              (setf (trackname (if track track (nth 16 (track-list *active-score*))))
                                               (read-string istream meta-length) )
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             (#x20 (when *mf-debug-info* (format t "~%not implemented MIDI Channel Prefix!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)))
                             ;-------end of track---------
                             (#x2F (when *mf-debug-info* (format t "~%endOfTrack"))
                              (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                               (make-instance 'endOfTrack :delta-time delta-time) )
                                   (setq ready t)
                              (loop repeat meta-length do (read-octet istream nil nil)))
                             ;-------set tempo---------
                             (#x51 (when *mf-debug-info* (format t "~%setTempo"))
                              (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                    (make-instance 'setTempo
                                                   :delta-time delta-time
                                                   :midi-tempo  (+ (ash (read-octet istream nil nil) 16)
                                                                    (ash (read-octet istream nil nil) 8)
                                                                    (read-octet istream nil nil))) )
                              (loop repeat (- meta-length 3) do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             (#x54 (when *mf-debug-info* (format t "~%not implemented SMPTE Offset!"))
                              (loop repeat meta-length do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             ;-------time signature---------
                             (#x58 (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                                    (make-instance 'timeSignature
                                                        :delta-time delta-time
                                                        :nn (read-octet istream nil nil)
                                                        :dd (read-octet istream nil nil)
                                                        :cc (read-octet istream nil nil)
                                                        :bb (read-octet istream nil nil))
                                         )
                              (loop repeat (- meta-length 4) do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             ;-------key signature---------
                             (#x59 (add-one-segment (if track track (nth 16 (track-list *active-score*)))
                                                    (make-instance 'keySignature
                                                        :delta-time delta-time
                                                        :sf (read-octet istream nil nil)
                                                        :mi (read-octet istream nil nil))
                                         )
                              (loop repeat (- meta-length 2) do (read-octet istream nil nil))
                              (incf delta-time (translate-variable-length-quantity istream)) )
                             ;-------other meta events---------
                             (t    (cond                                 ;; other Meta-Events
                                    ((and (>= meta-type #x01)
                                          (<= meta-type #x0F))
                                     (when *mf-debug-info* (format t "~%not implemented Text Event of type ~D!" meta-type)))
                                    ((= meta-type #x7F)
                                     (when *mf-debug-info* (format t "~%not implemented Sequencer-Specific Meta-Event!")))
                                    (t
                                     (when *mf-debug-info* 
                                         (format t "~%not implemented Meta-Event of type ~D!" meta-type))))
                                (loop repeat meta-length do (read-octet istream nil nil))
                                (incf delta-time (translate-variable-length-quantity istream)) ))))
                   (t    (error "wrong type of MTrk event"))))
            (t    (error "wrong type of MTrk event"))))
     ))



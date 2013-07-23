;;;-*-Mode: LISP; Package: DM -*-
;;
;; ***********************************************************
;;   Playing by computing intermediate lists of timed events
;;   that is sorted before output
;; ***********************************************************
;; 
;; send-before-noton not necessary before
;; Format of the generated list:
;; (<time> <channel> <message> <values to the message func>)
;; written by Anders Friberg  july 1987-.
;; 971115/af minor changes such as ifn => if (not 
;;2005-05-11/af changed all break-point playing so that it can end before the note
;;2005-05-12/af for staircase-bp-shape only the inserted points are played back
;;;061003/af added bank select
;;;061018/af added reverb and pan
;;;091013/af test new media player - problems with distribution on non-english systems......
;;;130722/af added simplified pedal: pedal-down, pedal-up

(in-package :dm)

;; Function that are possibly used outside
;;  playing? ()
;;  reset-time    ; reset-time is not in this file *****
;;  playlist-ask ()
;;  playlast-ask
;;  playlist
;;  generate-playlist
;;  playlist-midifile
;;  playlast
;;  playlistseq
;;  print-playlist


;; Other functions defined:
;;  play-list
;;  compare
;;  ask-for-start
;;  playlist-gen-list
;;  send-notegroupobj
;;  NoteGroupObj?
;;  send-before-noton
;;  send-after-noton

;;--------------------------------------------------------
;;--------- some utilities -------------------------------
;;--------------------------------------------------------

;; -------------
;;   PLAYING?
;; -------------
;;
(defun playing? ()
   "check if still playing in the midi driver"
   (let ((time (midi-get-time)))
      (if (> time (get-dm-var 'last-time))
         nil
         t )))

;;--------------------------------------------------------
;;--------- top level      -------------------------------
;;--------------------------------------------------------

;; ----------------
;;   PLAYLIST-ASK
;; ----------------
;;
(defun playlist-ask ()
  (set-dm-var 'last-list (sort (playlist-gen-list) 'compare))
  (play-list (get-dm-var 'last-list) t) )

;; ----------------
;;   PLAYLAST-ASK
;; ----------------
;;
(defun playlast-ask ()
  (play-list (get-dm-var 'last-list) t) )

;; ======================
;;   PLAYLIST, exported
;;
;(defun playlist ()
  ;;#+:mswindows
  ;(progn
  ; (real-time)
  ;  (set-dialog-field :real-time :tracktempo 100) 
  ;   (set-dialog-field :real-time :udc-instr 1) )
  ;; (setf *realtime-refnum* (midiopen "RealTime")) ***** should it be a different application ?
  ;; (midiconnect *realtime-refnum* (midigetnamedappl "MidiShare") t) )
;  (set-dm-var 'last-list (sort (playlist-gen-list) 'compare))
 ; (play-list (get-dm-var 'last-list) nil) 
 ;   )

#+:mcl
(defun playlist ()
  ;; Allocate a new MidiShare sequence
  (set-dm-var 'midishare-seq (midi-new-seq))
  
  ;; Fills the sequence with events
  (set-dm-var 'last-list (sort (playlist-gen-list) 'compare))
  (play-list (get-dm-var 'last-list) nil) 
  
  ;; Load the Player with the sequence and start playing
  (set-all-track-player (get-dm-var 'midishare-refnum) (get-dm-var 'midishare-seq) 500)
  (start-player (get-dm-var 'midishare-refnum))
  )
#+:mcl
(defun playlist ()

   ;;midi-open if not open
   (cond ((midishare)
          (cond ((not (get-dm-var 'midishare-refnum)) 
                 (midi-open)))
                )
         (t (advice "MidiShare not installed")) )

  ;; Allocate a new MidiShare sequence
  (set-dm-var 'midishare-seq (midi-new-seq))
  
  ;; Fills the sequence with events
  (set-dm-var 'last-list (sort (playlist-gen-list) 'compare))
  (play-list (get-dm-var 'last-list) nil) 
  
  ;; Load the Player with the sequence and start playing
  (set-all-track-player (get-dm-var 'midishare-refnum) (get-dm-var 'midishare-seq) 500)
  (start-player (get-dm-var 'midishare-refnum))
  )

;; ======================
;;   SLOPLIST, exported
;; Stops the MidiShare player

 #+:mcl
(defun stoplist()
  (stop-player (get-dm-var 'midishare-refnum)))

#+:mswindows
(defun playlist () (playlist-mplayer))

;; ===============================
;;   GENERATE-PLAYLIST, exported
;;
(defun generate-playlist ()
  (set-dm-var 'last-list
         (sort (playlist-gen-list) 'compare)) t)

;; ===============================
;;   PLAYLIST-MIDIFILE, exported
;;
(defun playlist-midifile ()
   (set-dm-var 'to-midi-file? t)
   (set-dm-var 'last-list
    (sort (playlist-gen-list) 'compare))
   ;(print (get-dm-var 'last-list))
   (play-list (get-dm-var 'last-list) nil)
   (set-dm-var 'to-midi-file? nil)
  )

;; ---------------------------------------------------------
;;   PLAYLIST-MPLAYER
;; play the score with mplayer
;; put the mplayer2 where dm starts
;; otherwise it will not be found

;;; saved to see how to start the media player
;;;   (sleep 0.5)
;;;   (win:sendmessage  (win:findwindow 
;;;                      "Mplayer" 
;;;                      "DMperformed.mid - Media Player (stopped)")
;;;                     #x111 #x1f5 0)
;;;
;;; Different old calling functions
;;;; testing w2000 - full pathname needed for mplayer2 and midifile in string
;;;    (win:winexec (concatenate 'string "mplayer " file) 1) ;acl 5.0.1
;;;    (excl:run-shell-command (concatenate 'string "mplayer " file) :wait nil) ;win 95 98?
;;;    (excl:run-shell-command 
;;;      (concatenate 'string "c:\\Program Files\\Windows Media Player\\mplayer2 " file) :wait nil)

;;; Borde vara den här (odokumenterade) funktionen i lispworks:
;;; WIN32:SHELL-EXECUTE
;;; Lambda-list: (WND LP-OPERATION LP-FILE LP-PARAMTERS LP-DIRECTORY N-SHOW-CMD)
;;;
;;; (sys:get-user-profile-directory) + local settings/Temp

#+(and :mswindows :allegro)
(defun playlist-mplayer-performed ()
  (let ((file 
         (namestring 
          (merge-pathnames (system:temporary-directory)
                           (concatenate 'string  
                             (pathname-name (score-filename *active-score*))
                             "-Perf-"
                             (GET-DATE-TIME-STRING)
                             ".mid")))))
    (save-performance-midifile1-fpath file)
    ;(excl:run-shell-command (concatenate 'string "mplayer2 " file) :wait nil)
    ;(excl:run-shell-command (concatenate 'string "C:\\Program Files\\Windows Media Player\\wmplayer " file) :wait nil)
    ;; funkar med lite tricks på win 7
    ;(print (concatenate 'string "java -jar C:\\af\\simip-2-standalone.jar " file))
    ;(excl:run-shell-command (concatenate 'string "java -jar C:\\af\\simip-2-standalone.jar " file))
    (excl:run-shell-command 
     ;(concatenate 'string "\"C:\\Program Files\\Java\\jre6\\bin\\java\" -jar C:\\af\\simip-4-standalone.jar 2 " file)
     ;;second argument gives midi device number (from zero)
     (concatenate 'string "\"C:\\Program Files\\Java\\jre7\\bin\\java\" -jar C:\\Nobackup\\afriberg\\simip-4-standalone.jar 3 " file)
     :wait nil)
    ))

#+(and :mswindows :allegro)
(defun playlist-mplayer-nominal ()
  (let ((file 
         (namestring 
          (merge-pathnames (system:temporary-directory)
                           (concatenate 'string  
                             (pathname-name (score-filename *active-score*))
                             "-Nom-"
                             (GET-DATE-TIME-STRING)
                             ".mid"))))) 
    (save-performance-midifile1-fpath file)
    ;(excl:run-shell-command (concatenate 'string "mplayer2 " file) :wait nil)
    ;(excl:run-shell-command (concatenate 'string "C:\\Program Files\\Windows Media Player\\wmplayer " file) :wait nil)
    (excl:run-shell-command 
     ;;second argument gives midi device number (from zero)
     (concatenate 'string "\"C:\\Program Files\\Java\\jre7\\bin\\java\" -jar C:\\Nobackup\\afriberg\\simip-4-standalone.jar 3 " file)
     :wait nil)
    ))


#+(and :mswindows :allegro)
(defun playlist-mplayer ()
  (with-waiting-cursor
  (let ((file 
         (namestring 
          (merge-pathnames (system:temporary-directory)
                           (concatenate 'string  
                             (pathname-name (score-filename *active-score*))
                             "-Play-"
                             (GET-DATE-TIME-STRING)
                             ".mid"))))) 
    (save-performance-midifile1-fpath file)
    ;(excl:run-shell-command (concatenate 'string "C:\\Program Files\\Windows Media Player\\wmplayer " file) :wait nil)
    (excl:run-shell-command 
     ;;second argument gives midi device number (from zero)
     (concatenate 'string "\"C:\\Program Files\\Java\\jre7\\bin\\java\" -jar C:\\Nobackup\\afriberg\\simip-4-standalone.jar 3 " file)
     :wait nil)
    )))


;;make a date and time string
;;added seconds
#+(and :mswindows :allegro)
(defun get-date-time-string ()
   (multiple-value-bind (sec min hour date month year) 
                       (get-decoded-time)
                       (format nil "~2,'0D~2,'0D~A-~A~2,'0D~2,'0D"
                         date month year hour min sec)
                       ))



;; ======================================================================
;;   PLAYLAST, exported
;;
#+:mswindows
(defun playlast ()
  (play-list (get-dm-var 'last-list) nil) )
 #+:mcl
(defun playlast ()
 (start-player (get-dm-var 'midishare-refnum)))

;; =========================
;;   PLAYLISTSEQ, exported
;;
(defun playlistseq ()
   (let ((trackl (track-list *active-score*))
         (activetracksi nil) )
      (loop for i from 0 to (1- (length trackl)) do  ;collect all active tracks index list
        (if (active-p (nth i trackl))
           (newr activetracksi i) ))
      (dolist (tracki activetracksi)                 ;deactivate all active tracks
         (setf (active-p (nth tracki trackl)) nil))
      (dolist (tracki activetracksi)                 ;activate one, play and deactivate
         (setf (active-p (nth tracki trackl)) t)   
         (playlist)
         (setf (active-p (nth tracki trackl)) nil) )   
      (dolist (tracki activetracksi)                 ;reactivate all active tracks
         (setf (active-p (nth tracki trackl)) t))
      ))

;; ============================
;;   PRINT-PLAYLIST, exported
;;
(defun print-playlist (&optional path)
    (pprint (get-dm-var 'last-list) (or path *standard-output*)) t)

;; =======================
;;   PLAY-LIST, internal
;;
;; called by playlist
;;
#|
(defun play-list (l ask?)
  "play the sorted list"
  (set-dm-var 'play-stop-p nil)
  (if (not (get-dm-var 'to-midi-file?))
       (progn
         ;(gc)
         (loop while (playing?))  ;wait if already playing
         (if (get-dm-var 'verbose) (print "play"))
         (if ask? (ask-for-start)) ))
  (let ((chan 1)(arglist)(time 0)(mess)(synt) ; (note-num 0)
        (addtime (if (not (get-dm-var 'to-midi-file?))
                     (+ (midi-get-time) (get-dm-var 'delay))
                    0))
        (sleep-time (/ (get-dm-var 'anticipation-time) 4000)) )
    (set-dm-var 'start-time addtime)
    (dolist (one l)
      (when (and (not (get-dm-var 'to-midi-file?)) (get-dm-var 'play-stop-p))
        (set-dm-var 'play-stop-p nil)
        (all-notes-off *active-score* time)
        (return))
      (setq time (+ addtime (round (* (get-dm-var 'DMtempo) (pop one))))) 
      (setq synt (pop one))
      (setq chan (pop one))
      (if (get-dm-var 'channel1?)
          (setq chan 1))
      (setf (channel synt) chan)
      (setq mess (pop one))
      (setq arglist (append (list mess synt) one (list time)))
       (if (not (get-dm-var 'to-midi-file?))
         (loop while (> time (+ (midi-get-time) (get-dm-var 'anticipation-time))) do
           (sleep sleep-time) ))
       ;(eval arglist)
       (apply  (car arglist) (cdr arglist))
       ;(apply  mess synt one time)
;;;       #+:mswindows
;;;     (progn
;;;       (process-single-event t) ;*****************
;;;        (set-dialog-field :real-time :txt-note-num (incf note-num)) ;*** CG code !!
;;;        )
       ;(print arglist) 
       )
    (if (not (get-dm-var 'to-midi-file?))
        (set-dm-var 'last-time time))
    ))

;;new version for GRAME
(defun play-list (l ask?)
  (let ((chan 1)(arglist)(time 0)(mess)(synt) ; (note-num 0)
        (addtime 0)
        )
    (dolist (one l)
      (setq time (+ addtime (round (pop one)))) 
      (setq synt (pop one))
      (setq chan (pop one))
      (setf (channel synt) chan)
      (setq mess (pop one))
      (setq arglist (append (list mess synt) one (list time)))
       (apply  (car arglist) (cdr arglist))
       )
    ))
|#
;;new version for GRAME
;;run through all events and put them in a sequence
(defun play-list (l ask?)
  (let ((chan 1)(arglist)(time 0)(mess)(synt) ; (note-num 0)
        (addtime 0)
        )
    ;;(print l)
    (dolist (one l)
      ;;(print time)
      (setq time (+ addtime (round (pop one)))) 
      (setq synt (pop one))
      (setq chan (pop one))
      (setf (channel synt) chan)
      (setq mess (pop one))
       (setq arglist (append (list mess synt) one (list time)))
      ;; (print (symbol-function (car arglist)))
   
      ;;(apply  (car arglist) (cdr arglist))
      (apply (symbol-function (car arglist)) (cdr arglist))
      ;;(print 'apply)
      )
    ))

;; =====================
;;   COMPARE, internal
;;
(defun compare (a b)
 (< (car a) (car b)) )

;; ===========================
;;   ASK-FOR-START, internal
;;
(defun ask-for-start ()
 (print "push return to start :")
 (read-char) )



;;--------------------------------------------------------
;;-------generate an unsorted list of the voices----------
;;--------------------------------------------------------

;; ======================
;;   playlist-gen-list, internal
;; ======================
;;
;;changed order of track-delay and send-track-var-before-play
(defun playlist-gen-list ()
   (if (not (get-dm-var 'to-midi-file?))
      (if (get-dm-var 'verbose)
         (format t "generate list")))
   (let ((startt)(endt)(l ())(chan 1)(tempo 1)
         (default-synt (make-synth (get-dm-var 'play-synth-name-default)))
          synt
         (active-p nil))
      (declare (special l startt tempo chan synt))
      (each-track
        (if (not (get-dm-var 'to-midi-file?))
           (setq startt 300 endt 300)
          (setq startt 0 endt 0))
        ;(incf startt (get-track-var 'track-delay))
        ;(incf endt (get-track-var 'track-delay))
        (if (get-track-var 'synth)
           (setq synt (get-track-var 'synth))
           (setq synt default-synt) )
        (if (get-track-var 'midi-channel)
           (setq chan (get-track-var 'midi-channel)))
       (send-track-var-before-play)
       
       ;(incf startt (get-track-var 'track-delay))
       ;(incf endt (get-track-var 'track-delay))
       ;;sätt till 1000 (500) - superfix for att fördröja första tonen när man spelar med roland jv1001 som tar minst 300ms! för att ställa om program change!
       ;(incf startt 1000)
       ;(incf endt 1000)

        (setq active-p t) ;(setq active-p nil) ;;deactivated the start-bar option
        (if (get-track-var 'active-p)
           (each-segment
;;;              (if (not active-p)
;;;                 (if (and (this 'bar) (>= (this 'bar) (get-dm-var 'start-bar)))
;;;                    (setq active-p t) ))
             (when active-p
                (setq startt endt)
                (setq endt (+ endt (this 'dr)))
                ;OTHER PARAMETERS
                (send-before-noton)
                (send-voice-par-before-noton)
                ;if REST
                (if (this 'rest)
                   (send-after-noton)   ;if chord on a rest
                   ;else if NOTEGROUP
                   (if (notegroupobj? (this 'n)) 
                      (send-notegroupobj)
                      ;else NOTE ON
                      (progn
                        (if (or (first?) (not (prev 'tie)))
                           (if (not (listp (this 'f0)))
                              (newr l (list (+ (round (/ startt tempo)) 0.1)  ;fix for sort routine
                                        synt chan 'note-on-sl (this-f0) (this 'sl)))  
                              (dolist (f0 (this 'f0))
                                 (newr l (list (+ (round (/ startt tempo)) 0.1)  ;fix for sort routine
                                           synt chan 'note-on-sl f0 (this 'sl))) )))
                        (send-after-noton)
                        ;and  NOTE OFF
                        (if (or (last?)
                                (and (get-dm-var 'send-note-off?) 
                                     (not (this 'tie))
                                     (not (this 'rest)) ))
                           (if (this 'dro)
                              (if (not (listp (this 'f0)))
                                 (newr l (list (round (/ (- endt (this 'dro)) tempo))
                                           synt chan 'note-on (this 'f0) 0) )
                                 (dolist (f0 (this 'f0))
                                    (newr l (list (round (/ (- endt (this 'dro)) tempo))
                                              synt chan 'note-on f0 0) )) )
                              (if (not (listp (this 'f0)))
                                 (newr l (list (round (/ endt tempo))
                                           synt chan 'note-on (this 'f0) 0) )
                                 (dolist (f0 (this 'f0))
                                    (newr l (list (round (/ endt tempo))
                                              synt chan 'note-on f0 0) )))
                              )))
                      ))))))
      (if (not (get-dm-var 'to-midi-file?))
         (if (get-dm-var 'verbose)
            (format t "~%  sort")))
      l
     ))

;; ===============================
;;   SEND-NOTEGROUPOBJ, internal
;; ==============================
;;
(defun send-notegroupobj ()
  (let ((nl (slot-value 'NoteList (this 'n))))
     (mapc
       '(lambda (NoteObj)
          (declare (special l startt tempo chan synt))
          (newr l (list         ;note on
            (+ (round (/ (+ startt (slot-value 'stt NoteObj)) tempo)) 0.1)  ;fix for sort routine
            chan synt 'note-on-da0 (slot-value 'f0 NoteObj) (slot-value 'da0 NoteObj) ))
          (newr l (list         ;note off
            (+ (round 
             (/ (+ startt (slot-value 'stt NoteObj)(slot-value 'dr NoteObj)) tempo)) 0.1)  ;fix for sort routine
            chan synt 'note-on (slot-value 'f0 NoteObj) 0 )))
        nl )))

;; ==============================
;;   NOTE-GROUP-OBJ-?, internal
;; ==============================
;;
(defun NoteGroupObj? (thing)
  (equal (type-of thing) 'notegroupobj) )

;(defun foo () (playlist-gen-list t))

;; ==============================
;;   SEND-BEFORE-PLAY
;; ==============================
;; send track parameters before any notes
;;
#|
(defun send-track-var-before-play ()
   (declare (special l startt tempo chan synt))
   ;PROGRAM
   (newr l (list (round (/ startt tempo))
             synt chan 'set-program (get-track-var 'midi-initial-program)))
   ;DELTA-CENT
   
   ;VIBRATO AMPLITUDE
   
   ;VIBRATO FREQUENCY
   
   ;VOLUME
   (newr l (list (round (/ startt tempo))
             synt chan 'set-vol (get-track-var 'midi-initial-volume)))
   
   ;PEDAL-PERF
   
  )
|#
(defun send-track-var-before-play ()
  (declare (special l startt tempo chan synt))
  ;BANK
  (if (get-track-var 'midi-bank-msb)
      (newr l (list (round (/ startt tempo)) synt chan 'set-bank-msb (get-track-var 'midi-bank-msb))) )
  (if (get-track-var 'midi-bank-lsb)
      (newr l (list (round (/ startt tempo)) synt chan 'set-bank-lsb (get-track-var 'midi-bank-lsb))) )
  ;PAN
  (if (get-track-var 'midi-pan)
      (newr l (list (round (/ startt tempo)) synt chan 'set-pan (get-track-var 'midi-pan))) )
  ;Reverb
  (if (get-track-var 'midi-reverb)
      (newr l (list (round (/ startt tempo)) synt chan 'set-reverb (get-track-var 'midi-reverb))) )
  ;PROGRAM
  (newr l (list (round (/ startt tempo)) synt chan 'set-program (get-track-var 'midi-initial-program)))
  ;DELTA-CENT
  
  ;VIBRATO AMPLITUDE
  
  ;VIBRATO FREQUENCY
  
  ;VOLUME
  (newr l (list (round (/ startt tempo)) synt chan 'set-vol (get-track-var 'midi-initial-volume)))
  
  ;PEDAL-PERF
  
  )


;; ===============================
;;   SEND-BEFORE-NOTON, internal
;; ==============================
;;
;; with vol instead of envelopes
;;
#|
(defun send-before-noton ()
  (declare (special l startt tempo chan synt))
  (if (get-dm-var 'send-before-note-on?)
    (let ((dc   (this 'dc))
          (va   (this 'va))
          (vf   (this 'vf))
          (prog (this 'pr))
          (vol  (this 'vol))
          (pedal-perf (this 'pedal-perf)) )
      ;(print "hej")
      ;PROGRAM
      (if prog (newr l (list (round (/ startt tempo)) synt chan 'set-program prog)))
      ;DELTA-CENT
      (if dc
        (if (listp dc)
          (loop while dc do
            ;(print "dc" dc)
            (newr l (list (round (/ (+ startt (pop dc)) tempo))
                          synt chan 'set-dc (pop dc) )))
          (newr l (list (round (/ startt tempo)) synt chan 'set-dc dc))) )
      ;VIBRATO AMPLITUDE
      (if va
        (if (listp va)
          (loop while va do
            ;(print "va" va)
            (newr l (list (round (/ (+ startt (pop va)) tempo))
                          synt chan 'set-va (pop va) )))
          (newr l (list (round (/ startt tempo)) synt chan 'set-va va))) )
      ;VIBRATO FREQUENCY
      (if vf
        (if (listp vf)
          (loop while vf do
            ;(print "vf" vf)
            (newr l (list (round (/ (+ startt (pop vf)) tempo))
                          synt chan 'set-vf (pop vf) )))
          (newr l (list (round (/ startt tempo)) synt chan 'set-vf vf))) )
      ;VOLUME
      (if vol
        (if (listp vol)
          (loop while vol do
            ;(print "vol" vol)
            (newr l (list (round (/ (+ startt (pop vol)) tempo))
                          synt chan 'set-vol (pop vol) )))
          (newr l (list (round (/ startt tempo)) synt chan 'set-vol vol))) )
      ;PEDAL-PERF
      (if pedal-perf
        (if (listp pedal-perf)
          (loop while pedal-perf do
            ;(print "pedal-perf" pedal-perf)
            (newr l (list (round (/ (+ startt (pop pedal-perf)) tempo))
                          synt chan 'set-pedal-perf (pop pedal-perf) )))
          (newr l (list (round (/ startt tempo)) synt chan 'set-pedal-perf pedal-perf))) )
      )))
|#

(defun send-before-noton ()
  (declare (special l startt tempo chan synt))
  ;(print "send-before")
  (if (get-dm-var 'send-before-note-on?)
    (let ((dc   (this 'dc))
          (va   (this 'va))
          (vf   (this 'vf))
          (prog (this 'pr))
          (bank-msb (this 'bank-msb))
          (bank-lsb (this 'bank-lsb))
          (pan (this 'pan))
          (reverb (this 'reverb))
          (vol  (this 'vol))
          (at  (this 'at)) ;attack time
          (pedal-perf (this 'pedal-perf))
          (pedal-down (this 'pedal-down))
          (pedal-up (this 'pedal-up))
          (shape-rate (get-dm-var 'PLAY-TIME-SHAPE-SAMPLING-RATE))
          )
      ;(print va)
      ;(print l)
      ;PROGRAM AND BANK
      (if prog (newr l (list (round (/ startt tempo)) synt chan 'set-program prog)))
      (if bank-msb (newr l (list (round (/ startt tempo)) synt chan 'set-bank-msb bank-msb)))
      (if bank-lsb (newr l (list (round (/ startt tempo)) synt chan 'set-bank-lsb bank-lsb)))
      ;PAN
      (if pan (newr l (list (round (/ startt tempo)) synt chan 'set-pan pan)))
      ;REVERB
      (if reverb (newr l (list (round (/ startt tempo)) synt chan 'set-reverb reverb)))
      ;ATTACK TIME
      (if at (newr l (list (round (/ startt tempo)) synt chan 'set-at at)))
      ;DELTA-CENT
;;;      (if dc
;;;        (if (typep dc 'shape)
;;;           (loop for time from 0 to (get-last-x-value dc) by shape-rate do
;;;            (newr l (list (round (/ (+ startt time) tempo))
;;;                          synt chan 'set-dc (get-value-at dc time) )))
;;;           (newr l (list (round (/ startt tempo)) synt chan 'set-dc dc)) ))
      (if dc
          (cond
           ((typep dc 'staircase-bp-shape)
            (loop for n from 0 to (1- (get-n-of-bp dc)) do
                  (newr l (list (round (/ (+ startt (get-x-value dc n)) tempo))
                                synt chan 'set-dc (get-y-value dc n) ))))
           ((typep dc 'shape)
            (loop for time from 0 to (get-last-x-value dc) by shape-rate do
                  (newr l (list (round (/ (+ startt time) tempo))
                                synt chan 'set-dc (get-value-at dc time) ))))
           (t (newr l (list (round (/ startt tempo)) synt chan 'set-dc dc)) )))
      ;VIBRATO AMPLITUDE
      (if va
        (if (typep va 'shape)
           (loop for time from 0 to (get-last-x-value va) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-va (get-value-at va time) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-va va)) ))
      ;VIBRATO FREQUENCY
      (if vf
        (if (typep vf 'shape)
           (loop for time from 0 to (get-last-x-value vf) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-vf (get-value-at vf time) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-vf vf)) ))
      ;VOLUME
      (if vol
        (if (typep vol 'shape)
           (loop for time from 0 to (get-last-x-value vol) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-vol (get-value-at vol time) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-vol vol)) ))
      ;PEDAL-PERF
      (if pedal-perf
        (if (typep pedal-perf 'shape)
           (loop for time from 0 to (get-last-x-value pedal-perf) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-pedal-perf (get-value-at pedal-perf time) )))
          (newr l (list (round (/ startt tempo)) synt chan 'set-pedal-perf pedal-perf)) ))
      (if pedal-down
           (newr l (list (+ pedal-down (round (/ startt tempo))) synt chan 'set-pedal-perf 1)) )
      (if pedal-up
           (newr l (list (+ pedal-up (round (/ startt tempo))) synt chan 'set-pedal-perf 0)) )
      )))

(defun send-voice-par-before-noton ()
  (declare (special l startt tempo chan synt))
  (if (get-dm-var 'send-before-note-on?)
    (let ((f1 (this 'f1))
          (f2 (this 'f2))
          (f3 (this 'f3))
          (f4 (this 'f4))
          (f5 (this 'f5))
          (shape-rate (get-dm-var 'PLAY-TIME-SHAPE-SAMPLING-RATE))
          )
      ;(print "hej")
      ;F1
      (if f1
        (if (typep f1 'shape)
           (loop for time from 0 to (get-x-max f1) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-f1 (round (get-value-at f1 time)) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-f1 f1)) ))
      ;F2
      (if f2
        (if (typep f2 'shape)
           (loop for time from 0 to (get-x-max f2) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-f2 (round (get-value-at f2 time)) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-f2 f2)) ))
      ;F3
      (if f3
        (if (typep f3 'shape)
           (loop for time from 0 to (get-x-max f3) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-f3 (round (get-value-at f3 time)) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-f3 f3)) ))
      ;F4
      (if f4
        (if (typep f4 'shape)
           (loop for time from 0 to (get-x-max f4) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-f4 (round (get-value-at f4 time)) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-f4 f4)) ))
      ;F5
      (if f5
        (if (typep f5 'shape)
           (loop for time from 0 to (get-x-max f5) by shape-rate do
            (newr l (list (round (/ (+ startt time) tempo))
                          synt chan 'set-f5 (round (get-value-at f5 time)) )))
           (newr l (list (round (/ startt tempo)) synt chan 'set-f5 f5)) ))
       
      )))
;; ==============================
;;   SEND-AFTER-NOTON, internal
;; ==============================
;;
;; for midifiles metaevents text type with chords examples: "F#" "Cm" "Dbdim"
;; phrase start: "PS456" = start on level 4, 5 and 6
;; phrase end: "PE456"
;;
;;0009/af changed so chord string is allowed
(defun send-after-noton ()
  (declare (special l startt tempo chan synt))
  (if (and (get-dm-var 'to-midi-file?)
           (get-dm-var 'midifile-out-save-chord-phrase?) )
    (let ((q   (this 'q))
          (phrase-start   (this 'phrase-start))
          (phrase-end   (this 'phrase-end)) )
      ;CHORD
      (if q (newr l (list (+ (round (/ startt tempo)) 0.2) ;after noton
                          synt chan 'meta-event-text 
                          (if (stringp q) q (chord-note-list-to-chordname q)))))
      ;PHRASE-START
      (if phrase-start 
        (let ((string ""))
          (dolist (nr phrase-start)
            (setq string (concatenate 'string string (prin1-to-string nr))))
          (newr l (list (+ (round (/ startt tempo)) 0.2)
                        synt chan 'meta-event-text
                        (concatenate 'string "PS" string) ))))
      ;PHRASE-END
      (if phrase-end 
        (let ((string ""))
          (dolist (nr phrase-end)
            (setq string (concatenate 'string string (prin1-to-string nr))))
          (newr l (list (+ (round (/ startt tempo)) 0.2)
                        synt chan 'meta-event-text
                        (concatenate 'string "PE" string) ))))
      )))


;; with accent analysis as control change messages (*10)
(defun send-after-noton ()
  (declare (special l startt tempo chan synt))
  (if (and (get-dm-var 'to-midi-file?)
           (get-dm-var 'midifile-out-save-chord-phrase?) )
    (let ((q   (this 'q))
          (phrase-start   (this 'phrase-start))
          (phrase-end   (this 'phrase-end))
          (accent-c   (this 'accent-c))
          (accent-m   (this 'accent-m))
          (accent-h   (this 'accent-h))
          )
      ;CHORD
      (if q (newr l (list (+ (round (/ startt tempo)) 0.2) ;after noton
                          synt chan 'meta-event-text 
                          (if (stringp q) q (chord-note-list-to-chordname q)))))
      ;PHRASE-START
      (if phrase-start 
        (let ((string ""))
          (dolist (nr phrase-start)
            (setq string (concatenate 'string string (prin1-to-string nr))))
          (newr l (list (+ (round (/ startt tempo)) 0.2)
                        synt chan 'meta-event-text
                        (concatenate 'string "PS" string) ))))
      ;PHRASE-END
      (if phrase-end 
        (let ((string ""))
          (dolist (nr phrase-end)
            (setq string (concatenate 'string string (prin1-to-string nr))))
          (newr l (list (+ (round (/ startt tempo)) 0.2)
                        synt chan 'meta-event-text
                        (concatenate 'string "PE" string) ))))
      (if accent-c
           (newr l (list (+ (round (/ startt tempo)) 0.2) synt chan 'set-accent-c (round (* 10 accent-c)))) )
      (if accent-m
          (newr l (list (+ (round (/ startt tempo)) 0.2) synt chan 'set-accent-m (round (* 10 accent-m)))) )
      (if accent-h
           (newr l (list (+ (round (/ startt tempo)) 0.2) synt chan 'set-accent-h (round (* 10 accent-h)))) )
      )))

;;eof

   
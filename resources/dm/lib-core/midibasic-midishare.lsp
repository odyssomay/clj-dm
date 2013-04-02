;;;-*-Mode: LISP; Package: DM -*-
;;
;; ****************************************************************
;;   Basic interface to the MIDI manager MIDISHARE for ACL Win 95
;; ****************************************************************
;;
;; Currently no error checking in 'midi-write-list
;; 9111/Anders Friberg
;; 9202 automatic connection to the apple midi driver added /af
;; 9701 ACL Win95 Midishare version /Vittorio Colombo
;; 9801 MidiShare toolbox controls added, see tutorial.lsp for MidiShare /vc
;;
;; another version of midibasic has been created using MIDI events
;; this version works at a lower level, MIDI bytes
;;
;; The following functions are defined:
;;  midi-open ()
;;  midi-close ()
;;  midi-get-time ()
;;  midi-write-list (list time)

(in-package :dm)

;; -------------
;;   MIDI-OPEN
;; -------------
;;
;; Open THE MidiShare application named "DM"
;;

(defun midi-open ()
   (cond ((midishare)
          (cond ((not (get-dm-var 'midishare-refnum)) 
                 (set-dm-var 'midishare-refnum (open-player "DM"))
                 (midiconnect (get-dm-var 'midishare-refnum) 0 -1)    ; (midigetnamedappl "MidiShare") t)
                 (set-dm-var 'last-time 0)
                 (advice "MIDI open"))
                (t (advice "MidiShare client for DM already open"))))
         (t (advice "MidiShare not installed")))
   )


;;------------
;; MIDI-CLOSE
;;------------
;;
(defun midi-close ()
   (cond ((midishare)
          (cond ((get-DM-var 'midishare-refnum)
                 (closeplayer (get-dm-var 'midishare-refnum))
                 (set-dm-var 'midishare-refnum nil)
                 (advice "MIDI closed"))
                (t (advice "MidiShare client for DM open is already closed"))))
         (t (advice "MidiShare not installed")))
   )


;;-----------
;; SEND-NOTE
;;-----------
;;from tutorial.lsp. We don't use it
;;
(defun send-note (pitch)
   (cond 
         ((get-dm-var 'midishare-refnum)
          (let ((event (midinewev typeNote)))
             (unless (= 0 event)
                (chan event 0)
                (port event (get-DM-var 'midishare-port)) ; set 1 if for soundboard output(or 0!!!)
             (pitch event pitch)
             (vel event 64)
             (dur event  1000)
             (midisendim (get-DM-var 'midishare-refnum) event))))
   (t (advice "Midishare not open"))))	


;;-----------
;; MIDI-TEST
;;-----------
;;
(defun midi-test ()
   (send-note 60)
   )
                  



;;; (defun midi-flush ())

;; -------------------
;;   MIDI-WRITE-LIST
;; -------------------
;; 
;; send notes: writes a list of midi bytes at time time
;; if output buffer is full it will try to send it again until it succeed
;;
;; the function 'stream-ev' creates an event of typeStrem
;;      
;;; (defun midi-write-list (list time)
;;;   (let ((event 0))   
;;;       (while (zerop event)      
;;;         (setq event (stream-ev       
;;;                      :fields list
;;;                      :port (get-DM-var 'midishare-port)
;;;                   )))
;;;       (midisendat (get-dm-var 'midishare-refnum) event time)))
;;; 
;;; ;new version for midifiles/af
;;; (defun midi-write-list (list time)
;;;    (if (get-dm-var 'to-midi-file?)
;;;       (midifile-write-list list time) ;defined in MIDIFileOutput
;;;       (let ((event 0))   
;;;          (while (zerop event)      
;;;            (setq event (stream-ev       
;;;                          :fields list
;;;                          :port (get-dm-var 'midishare-port)
;;;                          )))
;;;          ;(print-ll list "   " time)
;;;          (midisendat (get-dm-var 'midishare-refnum) event time))))

#|
(defun midi-write-list (list time)
   (if (get-dm-var 'to-midi-file?)
      (midifile-write-list list time) ;defined in MIDIFileOutput
      (let ((event (stream        
                         :fields list
                         :port (get-dm-var 'midishare-port)
                         )))
         ;(print-ll list "   " time)
        (midisendat (get-dm-var 'midishare-refnum) event time))))
|#

;;a new attempt - not tested with midishare
;;if it works there is no need to edit all syntobjects
;;20021211/af
(defun midi-write-list (list time)
   (if (get-dm-var 'to-midi-file?)
      (midifile-write-list list time) ;defined in MIDIFileOutput
      (let ((event (stream        
                    :fields list
                    :date time
                    :port (get-dm-var 'midishare-port)
                    )))
         ;(print-ll list "   " time)
        (midi-add-seq (get-dm-var 'midishare-seq) event))))

         
;; =======================================  
;; ------  for testing  ------------------
;; =======================================



;; -------------------------------
;;  midishare-connect-to-display
;; -------------------------------
;;  connect to the display if it is present
(defun midishare-connect-to-display ()
   (midiconnect (get-dm-var 'midishare-refnum) (midigetnamedappl "msDisplay") t))

   
#|
(defun foo()   ; it's not really working very well. Probably more bytes are required
  (let ((time (midigettime)))
    (midi-write-list '(#0x90 50 80) (+ 100 time))
    (midi-write-list '(#0x90 54 80) (+ 300 time) )
    (midi-write-list '(#0x91 57 80) (+ 300 time) )
    (midi-write-list '(#0x92 64 80) (+ 500 time) )
    (midi-write-list '(#0x93 67 80) (+ 500 time) )
    (midi-write-list '(#0x94 67 90) (+ 700 time) )
    ))

(defun midi-foo (list)
  (let ((event 0))   
      (while (zerop event)      
        (setq event (stream-ev       
                     :fields list
                     :port (get-DM-var 'midishare-port)
                  )))
      (midisendim (get-dm-var 'midishare-refnum) event)))

; (midi-foo '(#0x90 50 80))
|#

;;---------------------------------
;; DM-is-connected-to-Midishare-p
;;---------------------------------
;
(defun DM-is-connected-to-Midishare-p ()
   (if (eq (midiisconnected (get-DM-var 'midishare-refnum) 0) 1)
      (advice "DM is connected to Midishare")
      (advice "DM is NOT connected to Midishare")
      ))




;;--------------------
;; MidiShare-clients
;;--------------------
;; list MidiShare client applications
;
(defun MidiShare-clients ()
  (format t "List of MidiShare client applications ~%")
  (dotimes (i (midicountappls))
    (let ((ref (midigetindappl (1+ i))))
      (format t 
              " ~2d : reference number ~2d, name : '~a' ~%" 
              (1+ i) ref (midigetname ref))))) 








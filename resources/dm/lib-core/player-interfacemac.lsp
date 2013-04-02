;; =====================================================================================
;; The Player Library is Copyright (c) Grame, Computer Music Research Laboratory 
;; 1996-2000, and is distributed as Open Source software under the Artistic License;
;; see the file "Artistic" that is included in the distribution for details.
;;
;; Grame : Computer Music Research Laboratory
;; Web : http://www.grame.fr/Research
;; E-mail : MidiShare@rd.grame.fr
;; =====================================================================================

;; This file contains definitions for entry points of the Player library
;; It must be used with the Player shared library named "PlayerSharedPPC"
;; The PlayerSharedPPC file must be located either in the System Folder/Extension Folder
;; or in the same folder as the Player_Interface.lisp file.



(ccl::add-to-shared-library-search-path "PlayerSharedPPC")

;;===============================
;; Date structures ond constants
;;===============================

;;-------------------------------------------------------------------------- 
;; Player state   
;;-------------------------------------------------------------------------- 

(defparameter kIdle       0)
(defparameter kPause  	  1)
(defparameter kRecording  2)
(defparameter kPlaying    3)
(defparameter kWaiting    4)


;;-------------------------------------------------------------------------- 
;; Tracks state   
;;-------------------------------------------------------------------------- 

(defparameter kMaxTrack	256)
(defparameter kMuteOn  	1)
(defparameter kMuteOff  0)
(defparameter kSoloOn  	1)
(defparameter kSoloOff  0)
(defparameter kMute  	0)
(defparameter kSolo  	1)

;;-------------------------------------------------------------------------- 
;; Recording management  
;;-------------------------------------------------------------------------- 

(defparameter kNoTrack		-1)
(defparameter kEraseMode  	1)
(defparameter kMixMode 		0)

;;-------------------------------------------------------------------------- 
;; Loop  management  
;;-------------------------------------------------------------------------- 

(defparameter kLoopOn  	0)
(defparameter kLoopOff 	1)

;;-------------------------------------------------------------------------- 
;; Step Playing  
;;-------------------------------------------------------------------------- 

(defparameter kStepPlay  1)
(defparameter kStepMute  0)

;;-------------------------------------------------------------------------- 
;; Synchronisation  
;;-------------------------------------------------------------------------- 

(defparameter kInternalSync	0)
(defparameter kClockSync  	1)
(defparameter kSMPTESync 	2)
(defparameter kExternalSync 	3)

(defparameter kNoSyncOut	0)
(defparameter kClockSyncOut  	1)

;;-------------------------------------------------------------------------- 
;; MIDIfile  
;;-------------------------------------------------------------------------- 

(defparameter midifile0	 0)
(defparameter midifile1  1)
(defparameter midifile2  2)

(defparameter TicksPerQuarterNote	0)
(defparameter Smpte24			24)
(defparameter Smpte25			25)
(defparameter Smpte29			29)
(defparameter Smpte30			30)

;;-------------------------------------------------------------------------- 
;; Errors  : for the player
;;-------------------------------------------------------------------------- 
 
(defparameter PLAYERnoErr 			-1)		;; No error			            		 
(defparameter PLAYERerrAppl			-2)		;; Unable to open MidiShare application	 
(defparameter PLAYERerrEvent  		        -3)		;; No more MidiShare Memory 			 
(defparameter PLAYERerrMemory			-4)		;; No more Mac Memory 			       
(defparameter PLAYERerrSequencer		-5)		;; Sequencer error			          

 ;;-------------------------------------------------------------------------- 
 ;; Errors  :  for MidiFile
 ;;-------------------------------------------------------------------------- 

(defparameter noErr			0)		;; no error 						 
(defparameter ErrOpen			1)		;; file open error 	 
(defparameter ErrRead			2)		;; file read error	 
(defparameter ErrWrite		        3)		;; file write error	 
(defparameter ErrVol			4)		;; Volume error 	 
(defparameter ErrGetInfo 		5)		;; GetFInfo error	 
(defparameter ErrSetInfo		6)		;; SetFInfo error	 
(defparameter ErrMidiFileFormat	        7)	        ;; bad MidiFile format	 

;;================================
;; Record for position management
;;================================

(defrecord Pos
  (bar  :short)  
  (beat :short)     
  (unit :short))

(defmacro p-bar (e &optional (d nil d?))
  (if d?
    `(rset ,e :Pos.bar ,d)
    `(rref ,e :Pos.bar)))

(defmacro p-beat (e &optional (d nil d?))
  (if d?
    `(rset ,e :Pos.beat ,d)
    `(rref ,e :Pos.beat)))
    
(defmacro p-unit (e &optional (d nil d?))
  (if d?
    `(rset ,e :Pos.unit ,d)
    `(rref ,e :Pos.unit)))


;;==============================
;; Record for state management
;;==============================

(defrecord PlayerState
  (date  :longint)
  (tempo :longint)
  (tsnum :short)
  (tsdenom :short)
  (tsclick :short)
  (tsquarter :short)
  (bar  :short)   
  (beat :short)     
  (unit  :short)
  (state  :short)
  (syncin  :short)
  (syncout  :short))

(defmacro s-bar (e )
   `(rref ,e :PlayerState.bar))

(defmacro s-beat (e)
  `(rref ,e :PlayerState.beat))
 
(defmacro s-unit (e)
  `(rref ,e :PlayerState.unit))

(defmacro s-date (e )
  `(rref ,e :PlayerState.date))

(defmacro s-tempo (e)
  `(rref ,e :PlayerState.tempo))

(defmacro s-num (e)
  `(rref ,e :PlayerState.tsnum))

(defmacro s-denom (e)
  `(rref ,e :PlayerState.tsdenom))

(defmacro s-click (e )
  `(rref ,e :PlayerState.tsclick))

(defmacro s-quarter (e )
  `(rref ,e :PlayerState.tsquarter))

(defmacro s-state (e )
  `(rref ,e :PlayerState.state))

(defmacro s-syncin (e )
  `(rref ,e :PlayerState.syncin))

(defmacro s-syncout (e )
  `(rref ,e :PlayerState.syncout))


;;======================
;; Record for MidiFile
;;======================

(defrecord MidiFileInfos
  (format  :longint)     
  (timedef  :longint)   
  (clicks :longint)      
  (tracks  :longint)  )   

(defmacro mf-format (e &optional (d nil d?))
  (if d?
    `(rset ,e :MidiFileInfos.format ,d)
    `(rref ,e :MidiFileInfos.format)))
 
(defmacro mf-timedef (e &optional (d nil d?))
  (if d?
    `(rset ,e :MidiFileInfos.timedef ,d)
    `(rref ,e :MidiFileInfos.timedef)))

(defmacro mf-clicks (e &optional (d nil d?))
  (if d?
    `(rset ,e :MidiFileInfos.clicks ,d)
    `(rref ,e :MidiFileInfos.clicks)))

(defmacro mf-tracks (e )
  `(rref ,e :MidiFileInfos.tracks))

;;=============================
;; Interface to C entry points
;;=============================

(define-entry-point ( "Version" ("PlayerSharedPPC")) () :long)

(define-entry-point ( "OpenPlayer" ("PlayerSharedPPC")) ((name :ptr) ) :short)
(define-entry-point ( "ClosePlayer" ("PlayerSharedPPC")) ((refnum :short)))

(defun open-player (name)
  (with-pstrs ((pstr name))
    (openplayer pstr)))

;;===================
;; Transport control
;;===================

(define-entry-point ("StartPlayer" ("PlayerSharedPPC")) ((refnum :short)))
(defun start-player (ref) (StartPlayer ref))

(define-entry-point ("ContPlayer"  ("PlayerSharedPPC")) ((refnum :short)))
(define-entry-point ("StopPlayer"  ("PlayerSharedPPC")) ((refnum :short)))
(defun stop-player (ref) (StopPlayer ref))

(define-entry-point ("PausePlayer" ("PlayerSharedPPC")) ((refnum :short)))

;;===================
;; Record management
;;===================

(define-entry-point ("SetRecordModePlayer" ("PlayerSharedPPC"))  ((refnum :short) (state :short)))
(define-entry-point ("RecordPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum :short)))
(define-entry-point ("SetRecordFilterPlayer" ("PlayerSharedPPC"))  ((refnum :short) (filter :ptr)))

;;=====================
;; Position management
;;=====================

(define-entry-point ("SetPosBBUPlayer" ("PlayerSharedPPC")) ((refnum :short) (pos :ptr)))
(define-entry-point ("SetPosMsPlayer" ("PlayerSharedPPC")) ((refnum :short)  (date_ms :longint)))

;;==================
;; Loop management
;;==================

(define-entry-point ("SetLoopPlayer" ("PlayerSharedPPC")) ((refnum :short) (state :short)))
(define-entry-point ("SetLoopStartBBUPlayer" ("PlayerSharedPPC")) ((refnum :short) (pos :ptr)) :long)
(define-entry-point ("SetLoopEndBBUPlayer" ("PlayerSharedPPC")) ((refnum :short) (pos :ptr)) :long)
(define-entry-point ("SetLoopStartMsPlayer"("PlayerSharedPPC")) ((refnum :short)  (date_ms :longint)) :long)
(define-entry-point ("SetLoopEndMsPlayer" ("PlayerSharedPPC")) ((refnum :short)  (date_ms :longint)) :long)

;;============================
;; Synchronisation management
;;============================

(define-entry-point ("SetSynchroInPlayer" ("PlayerSharedPPC")) ((refnum :short) (state :short)))
(define-entry-point ("SetSynchroOutPlayer" ("PlayerSharedPPC")) ((refnum :short) (state :short)))
(define-entry-point ("SetSMPTEOffsetPlayer" ("PlayerSharedPPC")) ((refnum :short) (smptepos :ptr)))
(define-entry-point ("SetTempoPlayer" ("PlayerSharedPPC")) ((refnum :short) (tempo :longint)))

;;===================
;; State management
;;===================

(define-entry-point ("GetStatePlayer" ("PlayerSharedPPC")) ((refnum :short) (playerstate :ptr )))
(define-entry-point ("GetEndScorePlayer" ("PlayerSharedPPC")) ((refnum :short) (playerstate :ptr )))

;;==============
;; Step playing 
;;==============

(define-entry-point ("ForwardStepPlayer" ("PlayerSharedPPC")) ((refnum :short) (flag :short)))
(define-entry-point ("BackwardStepPlayer" ("PlayerSharedPPC")) ((refnum :short) (flag :short)))

;;====================
;; Tracks management
;;====================

(define-entry-point ("GetAllTrackPlayer" ("PlayerSharedPPC")) ((refnum :short)) :ptr)
(define-entry-point ("GetTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum :short)) :ptr)

(define-entry-point ("SetTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum :short) (seq :ptr)) :long)
(define-entry-point ("SetAllTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (seq :ptr) (ticks_per_quarter :long)) :long)
(defun set-all-track-player (ref ptr tpq) (SetAllTrackPlayer ref ptr tpq))

(define-entry-point ("SetParamPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum short) (param short) (value short )))
(define-entry-point ("GetParamPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum short) (param short)) :short )

(define-entry-point ("InsertAllTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (seq :ptr)) :long)
(define-entry-point ("InsertTrackPlayer" ("PlayerSharedPPC")) ((refnum :short) (tracknum short)(seq :ptr)) :long)

;;====================
;; Midifile management
;;====================

(define-entry-point ("MidiFileSave" ("PlayerSharedPPC")) (( name :ptr) (seq :ptr) (infos :ptr)) :long)
(define-entry-point ("MidiFileLoad" ("PlayerSharedPPC")) (( name :ptr) (seq :ptr) (infos :ptr)) :long)

(defun midi-file-load (name seq info)
  (with-cstrs ((cstr name))
    (MidiFileLoad cstr seq info)))

(defun midi-file-save (name seq info)
  (with-cstrs ((cstr name))
    (MidiFileSave cstr seq info)))



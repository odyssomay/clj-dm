;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  MidiShare-interface.lisp
;;
;;  Copyright (c) 1990,2001 GRAME.  All rights reserved.
;;
;;  This file contains definitions for records and Pascal style routines, used
;;  for interfacing ACL with MidiShare 1.31, real-time multitasking Midi operating system.
;;  It is in conformity with MPW Pascal MidiShareUnit.p .
;;
;;  History :
;;  
;;   11-Nov-90, First version. -Yo-
;;   25-Nov-90, Ajoute def de TMidiSeq + FirstEv & LastEv -Yo-
;;   25-Nov-90, Continue change en Cont -Yo-
;;   26-Nov-90, Modification de firstEv, lastEv, link,
;;              on ne pouvais pas ecrire par ex: (firstEv seq nil) qui etait 
;;              confondu avec (firstEv seq)
;;   01-Dec-90, Ajout d'une macro DOEVENTS, analogue a DOLIST, pour parcourir
;;              une chaine d'evenements.-Yo-
;;              Ajout des fonctions : Clock, typeName, name, fieldslist,
;;              printEv, printSeq. -Yo-
;;              Ajout des fonctions ou macro : pushevent, dupevents, delevents,
;;              mergeevents. -Yo-
;;   07-Dec-90, Correction de ProgChange. -Yo-
;;   12-Dec-90  Ajout de linkSE,linkST
;;-------------------------------------------------------------------------
;;   15-Dec-90  Nouvelle version de l'interface, restreinte aux seules
;;              fonctions de MidiShare et utilisant des macros. -Yo- 
;;   09-Jan-91  Ajout d'une variante info dans la description d'un evenement et des
;;              fonctions d'acces associees.
;;   09-Jan-91  Ajout fonctions d'acces aux filtres
;;   14-Mai-91  Adaptation MCL 2.0b1
;;   19-Mai-91  Pb des ff-call. Enrobage par (Block nil ..)
;;   22-Mai-91  Changement de nom des macro d'acces aux filtres
;;   31-Mai-91  Ajout des "s", (eval-when () ...)
;;   18-Jul-91  Ajout de la fonction bend (de l'ancienne version de msh-interface)
;;   04-Aou-91  Toutes les macros d'acces transferees dans le fichier extension
;;   31-Oct-91  Modification de MidiForgetTask
;;-------------------------------------------------------------------------
;;   04-Dec-94  Suppression du package MidiShare !!!
;;		Suppression des (block nil ..)
;;-------------------------------------------------------------------------
;;   22-07-96   Adaptation pour MCL PPC 3.9 : Le fonctionnement de ff-call a change
;;		pour les fonctions Pascal, il ne faut plus pusher dans la pile la place
;;		pour le resultat !!!
;;   23-07-96   Integration du fichiers "0 - quit-actions.lisp" et d'une partie du 
;;		fichier "2 - MidiShare-Extension.lisp"
;;   13-04-01   Ajout du type PortPrefix 
;;   15-06-01   Ajout des fonctions de gestion du filtre
;;   19-06-01   Changement du fonctionnement des fonctions de connection et etat des filtres 
;;              pour rendre le code multi-platorm
;;   24-06-01   Correction MidiForgetTask
;;   25-06-01   add-startup-action et add-quit-action changees en fonctions
;;   27-06-01   Ajout du code correspondant a l'interface Linux 
;;   05-07-01   Nettoyage
;;   07-01-02   Ajout des fonctions de gestion des drivers sur Macintosh



;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 				MidiShare Constant Definitions
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------


;; Constant definition for every type of MidiShare events

(defconstant typeNote 0          "a note with pitch, velocity and duration")
(defconstant typeKeyOn 1         "a key on with pitch and velocity")
(defconstant typeKeyOff 2        "a key off with pitch and velocity")
(defconstant typeKeyPress 3      "a key pressure with pitch and pressure value")
(defconstant typeCtrlChange 4    "a control change with control number and control value")
(defconstant typeProgChange 5    "a program change with program number")
(defconstant typeChanPress 6     "a channel pressure with pressure value")
(defconstant typePitchWheel 7    "a pitch bender with lsb and msb of the 14-bit value")
(defconstant typePitchBend 7     "a pitch bender with lsb and msb of the 14-bit value")
(defconstant typeSongPos 8       "a song position with lsb and msb of the 14-bit position")
(defconstant typeSongSel 9       "a song selection with a song number")
(defconstant typeClock 10        "a clock request (no argument)")
(defconstant typeStart 11        "a start request (no argument)")
(defconstant typeContinue 12     "a continue request (no argument)")
(defconstant typeStop 13         "a stop request (no argument)")
(defconstant typeTune 14         "a tune request (no argument)")
(defconstant typeActiveSens 15   "an active sensing code (no argument)")
(defconstant typeReset 16        "a reset request (no argument)")
(defconstant typeSysEx 17        "a system exclusive with any number of data bytes. Leading $F0 and tailing $F7 are automatically supplied by MidiShare and MUST NOT be included by the user")
(defconstant typeStream 18       "a special event with any number of data and status bytes sended without any processing")
(defconstant typePrivate 19      "a private event for internal use with 4 32-bits arguments")
(defconstant typeProcess 128     "an interrupt level task with a function adress and 3 32-bits arguments")
(defconstant typeDProcess 129    "a foreground level task with a function adress and 3 32-bits arguments")
(defconstant typeQFrame 130      "a quarter frame message with a type from 0 to 7 and a value")


(defconstant typeCtrl14b	131)
(defconstant typeNonRegParam	132)
(defconstant typeRegParam	133)

(defconstant typeSeqNum		134)
(defconstant typeTextual	135)
(defconstant typeCopyright	136)
(defconstant typeSeqName	137)
(defconstant typeInstrName	138)
(defconstant typeLyric		139)
(defconstant typeMarker		140)
(defconstant typeCuePoint	141)
(defconstant typeChanPrefix	142)
(defconstant typeEndTrack	143)
(defconstant typeTempo		144)
(defconstant typeSMPTEOffset	145)

(defconstant typeTimeSign	146)
(defconstant typeKeySign	147)
(defconstant typeSpecific	148)
(defconstant typePortPrefix	149)

(defconstant typeRcvAlarm	150)
(defconstant typeApplAlarm	151)


(defconstant typeReserved 152    "events reserved for futur use")
(defconstant typedead 255        "a dead task. Used by MidiShare to forget and inactivate typeProcess and typeDProcess tasks")


;; Constant definition for every MidiShare error code

(defconstant MIDIerrSpace -1	 "too many applications")
(defconstant MIDIerrRefNum -2	 "bad reference number")
(defconstant MIDIerrBadType -3   "bad event type")
(defconstant MIDIerrIndex -4	 "bad index")


;; Constant definition for the Macintosh serial ports

(defconstant ModemPort 0	 "Macintosh modem port")
(defconstant PrinterPort 1	 "Macintosh printer port")


;; Constant definition for the synchronisation modes

(defconstant MidiExternalSync #x8000	 "Bit-15 set for external synchronisation")
(defconstant MidiSyncAnyPort #x4000	 "Bit-14 set for synchronisation on any port")


;; Constant definition for SMPTE frame format

(defconstant smpte24fr 0	 	"24 frame/sec")
(defconstant smpte25fr 1	 	"25 frame/sec")
(defconstant smpte29fr 2	 	"29 frame/sec (30 drop frame)")
(defconstant smpte30fr 3	 	"30 frame/sec")


;; Constant definition for MidiShare world changes

(defconstant MIDIOpenAppl 1      "an application was opened")
(defconstant MIDICloseAppl 2     "an application was closed")
(defconstant MIDIChgName 3       "an application name was changed")
(defconstant MIDIChgConnect 4    "a connection was changed")
(defconstant MIDIOpenModem 5     "Modem port was opened")     ;; Now obsolete
(defconstant MIDICloseModem 6    "Modem port was closed")     ;; Now obsolete
(defconstant MIDIOpenPrinter 7   "Printer port was opened")
(defconstant MIDIClosePrinter 8  "Printer port was closed")
(defconstant MIDISyncStart 9     "SMPTE synchronisation just start")
(defconstant MIDISyncStop 10     "SMPTE synchronisation just stop")
(defconstant MIDIChangeSync 10 )

(defconstant MIDIOpenDriver 11      )
(defconstant MIDICloseDriver 12     )
(defconstant MIDIAddSlot 13         )
(defconstant MIDIRemoveSlot 14      )
(defconstant MIDIChgSlotConnect 15  )
(defconstant MIDIChgSlotName 16     )


;;---------------------------------------------------------------------------------
;; 			    To Add Startup and Quit Actions
;;---------------------------------------------------------------------------------

(unless (and (boundp '*lisp-startup-functions*)  (boundp '*lisp-cleanup-functions*))
	(defvar *lisp-startup-functions* nil)
	(defvar *lisp-cleanup-functions* nil)
)

;;................................................................................: add-startup-action
(defun add-startup-action (fun)
  (pushnew fun *lisp-startup-functions*))

;;................................................................................: add-quit-action
(defun add-quit-action (fun)
  (pushnew fun *lisp-cleanup-functions*))



;;---------------------------------------------------------------------------------
;; 			    Interface for MCL on Macintosh
;;---------------------------------------------------------------------------------

#+(and apple mcl powerpc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*warn-if-redefine* nil))
    (require :ff)))

#+(and apple mcl powerpc)

;;---------------------------------------------------------------------------------
;;
;; 				Utilities
;;
;;---------------------------------------------------------------------------------

(progn

  (defun %%get-string (ps) 
    "Same as %get-string but work with mac non-zone pointers"
    (let (name count)
      (setq count (%get-byte ps))
      (setq name (make-string count))  
      (dotimes (i count)
        (setq ps (%inc-ptr ps 1))
        (setf (aref name i) (coerce (%get-byte ps) 'character)))
      name))

  ;; For bug (?) in MCL PPC 3.9 when returning signed word
  
  (defun %%unsigned-to-signed-word (w)
    "convert an unsigned word to a signed word"
    (if (< w 32768) w (- w 65536)))
  
  (defun %%word-high-byte (w)
    "most significant byte of a word"
    (ash w -8))
  
  (defun nullptrp (p) (%null-ptr-p p))
  
  (defun nullptr () (%null-ptr))

;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 				MidiShare Data Structures
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------


;; Extension record for typeSysEx events

(defrecord TMidiSEX  
  (link (:pointer TMidiSEX))
  (data (:array :byte 12)))


;; Extension record for typePrivate, typeProcess and typeDProcess events

(defrecord TMidiST
  (ptr1 :pointer)
  (ptr2 :pointer)
  (ptr3 :pointer)
  (ptr4 :pointer))


;;---------------------------------------------------------------------------------
;; Common Record for all MidiShare events
;;---------------------------------------------------------------------------------

(defrecord TMidiEv
  (link (:pointer TMidiEv))
  (date :longint)
  (evtype :byte)
  (ref :byte)
  (port :byte)
  (chan :byte)
  (variant ((pitch :byte)
            (vel :byte)
            (dur :integer))
           ((data0 :byte)
            (data1 :byte)
            (data2 :byte)
            (data3 :byte))
           ((info :longint))
           ((linkSE (:pointer TMidiSEX)))
           ((linkST (:pointer TMidiST)))))


;;---------------------------------------------------------------------------------
;; Record for a MidiShare Sequence
;;---------------------------------------------------------------------------------

(defrecord TMidiSeq
  (first (:pointer TMidiEv))    ; first event
  (last (:pointer TMidiEv))     ; last event
  (undef1 :pointer)   
  (undef2 :pointer) )  


;;---------------------------------------------------------------------------------
;; Record for MidiShare SMPTE synchronisation informations
;;---------------------------------------------------------------------------------

(defrecord TSyncInfo
  (time :longint)
  (reenter :longint)
  (syncMode :unsigned-short)
  (syncLocked :byte)
  (syncPort :byte)
  (syncStart :longint)
  (syncStop :longint)
  (syncOffset :longint)
  (syncSpeed :longint)
  (syncBreaks :longint)
  (syncFormat :short))


;;---------------------------------------------------------------------------------
;; Record for MidiShare SMPTE locations
;;---------------------------------------------------------------------------------

(defrecord TSmpteLocation
  (format :short)
  (hours :short)
  (minutes :short)
  (seconds :short)
  (frames :short)
  (fracs :short))

;;---------------------------------------------------------------------------------
;; Constants and records for MidiShare drivers 
;;---------------------------------------------------------------------------------

(defconstant MidiInputSlot 1)
(defconstant MidiOutputSlot 2)
(defconstant MidiInputOutputSlot 3)

(defrecord TSlotRefNum
  (drvRef :short)
  (slotRef :short))
 
(defrecord TSlotInfos
  (name (string 31))
  (direction :byte)
  (cnx (string 31))
  (reserved0 :longint)
  (reserved1 :longint))
  
 
(defrecord TDriverInfos
  (name (string 31))
  (version :short)
  (slots :short)
  (reserved0 :longint)
  (reserved1 :longint))


;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 		Macros for accessing MidiShare Events data structures
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

;;---------------------------------------------------------------------------------
;;                      Macros common to every type of event
;;---------------------------------------------------------------------------------

;;................................................................................: link
(defmacro link (e &optional (d nil d?))
"read or set the link of an event"
  (if d?
    `(rset ,e :TMidiEv.link ,d)
    `(rref ,e :TMidiEv.link)))

;;................................................................................: date
(defmacro date (e &optional d)
"read or set the date of an event"
  (if d
    `(rset ,e :TMidiEv.date ,d)
    `(rref ,e :TMidiEv.date)))

;;................................................................................: type
(defmacro type (e &optional v)
"read or set the type of an event. Be careful in 
 modifying the type of an event"
  (if v
    `(rset ,e :TMidiEv.evType ,v)
    `(rref ,e :TMidiEv.evType)))

;;................................................................................: ref
(defmacro ref (e &optional v)
"read or set the reference number of an event"
  (if v
    `(rset ,e :TMidiEv.ref ,v)
    `(rref ,e :TMidiEv.ref)))

;;................................................................................: port
(defmacro port (e &optional v)
"read or set the port number of an event"
  (if v
    `(rset ,e :TMidiEv.port ,v)
    `(rref ,e :TMidiEv.port)))

;;................................................................................: chan
(defmacro chan (e &optional v)
"read or set the chan number of an event"
  (if v
    `(rset ,e :TMidiEv.chan ,v)
    `(rref ,e :TMidiEv.chan)))

;;................................................................................: field
(defmacro field (e &optional f v)
"give the number of fields or read or set a particular field of an event"
  (if f
    (if v
      `(midiSetField ,e ,f ,v)
      `(midiGetField ,e ,f))
    `(midiCountFields ,e)))

;;................................................................................: fieldsList
(defun fieldsList (e &optional (n 4))
"collect all the fields of an event into a list"
  (let (l)
    (dotimes (i (min n (midicountfields e)))
      (push (midigetfield e i) l))
    (nreverse l)))


;;---------------------------------------------------------------------------------
;;                         Specific to typeNote events
;;---------------------------------------------------------------------------------

;;................................................................................: pitch
(defmacro pitch (e &optional v)
"read or set the pitch of an event"
  (if v
    `(rset ,e :TMidiEv.pitch ,v)
    `(rref ,e :TMidiEv.pitch)))

;;................................................................................: vel
(defmacro vel (e &optional v)
"read or set the velocity of an event"
  (if v
    `(rset ,e :TMidiEv.vel ,v)
    `(rref ,e :TMidiEv.vel)))

;;................................................................................: dur
(defmacro dur (e &optional v)
"read or set the duration of an event"
  (if v
    `(rset ,e :TMidiEv.dur ,v)
    `(rref ,e :TMidiEv.dur)))


;;---------------------------------------------------------------------------------
;;                        Specific to other types of events
;;---------------------------------------------------------------------------------

;;................................................................................: linkSE
(defmacro linkSE (e &optional (d nil d?))
"read or set the link of an SEXevent "
  (if d?
    `(rset ,e :TMidiEv.linkSE ,d)
    `(rref ,e :TMidiEv.linkSE)))

;;................................................................................: linkST
(defmacro linkST (e &optional (d nil d?))
 "read or set the link of an STevent "
  (if d?
    `(rset ,e :TMidiEv.linkST ,d)
    `(rref ,e :TMidiEv.linkST)))


;;................................................................................: kpress
(defmacro kpress (e &optional v)
  (if v
    `(rset ,e :TMidiEv.vel ,v)
    `(rref ,e :TMidiEv.vel)))


;;................................................................................: ctrl
(defmacro ctrl (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: param
(defmacro param (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: num
(defmacro num (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: prefix
(defmacro prefix (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: tempo
(defmacro tempo (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: seconds
(defmacro seconds (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: subframes
(defmacro subframes (e &optional v)
  (if v
    `(midisetfield ,e 1 ,v)
    `(midigetfield ,e 1)))


;;................................................................................: val
(defmacro val (e &optional v)
  (if v
    `(midisetfield ,e 1 ,v)
    `(midigetfield ,e 1)))


;;................................................................................: pgm
(defmacro pgm (e &optional v)
  (if v
    `(rset ,e :TMidiEv.pitch ,v)
    `(rref ,e :TMidiEv.pitch)))


;;................................................................................: bend
(defmacro bend (e &optional v)
  "read or set the bend value of an event"
  (if v
    `(multiple-value-bind (ms7b ls7b) (floor (+ ,v 8192) 128)
       (rset ,e :TMidiEv.pitch ls7b)
       (rset ,e :TMidiEv.vel ms7b))
    `(- (+ (rref ,e :TMidiEv.pitch) (* 128 (rref ,e :TMidiEv.vel))) 8192)))


;;................................................................................: clk
(defmacro clk (e &optional v)
  (if v
    `(multiple-value-bind (ms7b ls7b) (floor (round (/ ,v 6)) 128)
       (rset ,e :TMidiEv.pitch ls7b)
       (rset ,e :TMidiEv.vel ms7b))
    `(* 6 (+ (pitch ,e) (* 128 (vel ,e)))) ))


;;................................................................................: song
(defmacro song (e &optional v)
  (if v
    `(rset ,e :TMidiEv.pitch ,v)
    `(rref ,e :TMidiEv.pitch)))


;;................................................................................: fields
(defmacro fields (e &optional v)
  (if v
    `(let ((e ,e)) (mapc #'(lambda (f) (midiaddfield e f)) ,v))
    `(let (l (e ,e))  (dotimes (i (midicountfields e)) (push (midigetfield e i) l)) (nreverse l)) ))


;;................................................................................: text
(defmacro text (e &optional s)
  (if s
    `(fields ,e (map 'list #'char-code ,s))
    `(map 'string #'character (fields ,e)) ))


;;................................................................................: fmsg
(defmacro fmsg (e &optional v)
  (if v
    `(rset ,e :TMidiEv.pitch ,v)
    `(rref ,e :TMidiEv.pitch)))

;;................................................................................: fcount
(defmacro fcount (e &optional v)
  (if v
    `(rset ,e :TMidiEv.vel ,v)
    `(rref ,e :TMidiEv.vel)))

;;................................................................................: tsnum
(defmacro tsnum (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: tsdenom
(defmacro tsdenom (e &optional v)
  (if v
    `(midisetfield ,e 1 ,v)
    `(midigetfield ,e 1)))


;;................................................................................: tsclick
(defmacro tsclick (e &optional v)
  (if v
    `(midisetfield ,e 2 ,v)
    `(midigetfield ,e 2)))

;;................................................................................: tsquarter
(defmacro tsquarter (e &optional v)
  (if v
    `(midisetfield ,e 3 ,v)
    `(midigetfield ,e 3)))

;;................................................................................: alteration
(defmacro alteration (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))

;;................................................................................: minor-scale
(defmacro minor-scale (e &optional v)
  (if v
    `(midisetfield ,e 1 (if ,v 1 0))
    `(= 1 (midigetfield ,e 1))))

;;................................................................................: info
(defmacro info (e &optional d)
"read or set the info of an event"
  (if d
    `(rset ,e :TMidiEv.info ,d)
    `(rref ,e :TMidiEv.info)))


;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 		Macros for accessing MidiShare Sequences data structures
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

;;................................................................................: firstEv
(defmacro firstEv (s &optional (e nil e?))
"read or set the first event of a sequence"
  (if e?
    `(rset ,s :TMidiSeq.first ,e)
    `(rref ,s :TMidiSeq.first)))

;;................................................................................: lastEv
(defmacro lastEv (s &optional (e nil e?))
"read or set the last event of a sequence"
  (if e?
    `(rset ,s :TMidiSeq.last ,e)
    `(rref ,s :TMidiSeq.last)))


;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 				MidiShare Entry Points
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

;; Interface description for a MidiShare PROCEDURE 
;; with a word and a pointer parameter
;;  (ff-call *midiShare* :word <arg1> :ptr <arg2> :d0 <MidiShare routine #>)
;;
;; Interface description for a MidiShare FUNCTION (previous to MCL PPC 3.9)
;; with a word and a pointer parameter and a pointer result
;;  (ff-call *midiShare*  :ptr (%null-ptr) :word <arg1> :ptr <arg2> :d0 <MidiShare routine #> :ptr)
;;
;; Interface description for a MidiShare FUNCTION (with MCL PPC 3.9) 
;; with a word and a pointer parameter and a pointer result
;;  (ff-call *midiShare* :word <arg1> :ptr <arg2> :d0 <MidiShare routine #> :ptr)
;;


;; Entry point of MidiShare (setup at boot time by the "MidiShare" init)

(defvar *midiShare*)

;;---------------------------------------------------------------------------------
;;			To Know about MidiShare and Active Sessions
;;---------------------------------------------------------------------------------

;;................................................................................: MidiShare
(defun MidiShare ()
"returns true if MidiShare is installed"
  (and (= (%get-word *midiShare*) #xD080)
       (= (%get-word *midiShare* 2) #xD080)))

;;................................................................................: MidiGetVersion
(defmacro MidiGetVersion ()
"Give MidiShare version as a fixnum. For example 131 as result, means : version 1.31"
  `(ff-call *midiShare* :d0 0 :word))

;;................................................................................: MidiCountAppls
(defmacro MidiCountAppls ()
"Give the number of MidiShare applications currently opened"
  `(ff-call *midiShare* :d0 1 :word))

;;................................................................................: MidiGetIndAppl
(defmacro MidiGetIndAppl (index)
"Give the reference number of a MidiShare application from its index, a fixnum
 between 1 and (MidiCountAppls)"
  `(%%unsigned-to-signed-word (ff-call *midiShare* :word ,index :d0 2 :word)))

;;................................................................................: MidiGetNamedAppl
(defmacro MidiGetNamedAppl (name)
"Give the reference number of a MidiShare application from its name"
  `(with-pstrs ((s ,name))
    (%%unsigned-to-signed-word (ff-call *midiShare* :ptr s :d0 3 :word))))

;;---------------------------------------------------------------------------------
;;			To Open and Close a MidiShare session
;;---------------------------------------------------------------------------------

;;................................................................................: MidiOpen
(defmacro MidiOpen (name)
"Open a new MidiShare application, with name name. Give a unique reference number."
  `(with-pstrs ((s ,name))
    (%%unsigned-to-signed-word (ff-call *midiShare* :ptr s :d0 4 :word))))

;;................................................................................: MidiClose
(defmacro MidiClose (refNum)
"Close an opened MidiShare application from its reference number"
  `(ff-call *midiShare* :word ,refNum :d0 5))


;;---------------------------------------------------------------------------------
;;			To Configure a MidiShare session
;;---------------------------------------------------------------------------------

;;................................................................................: MidiGetName
(defmacro MidiGetName (refNum)
"Give the name of a MidiShare application from its reference number"
    `(%%get-string (ff-call *midiShare* :word ,refNum :d0 6 :ptr)))

;;................................................................................: MidiSetName
(defmacro MidiSetName (refNum name)
"Change the name of a MidiShare application"
  `(with-pstrs ((s ,name))
    (ff-call *midiShare* :word ,refNum :ptr s :d0 7 )))

;;................................................................................: MidiGetInfo
(defmacro MidiGetInfo (refNum)
"Give the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon."
  `(ff-call *midiShare* :word ,refNum :d0 8 :ptr))

;;................................................................................: MidiSetInfo
(defmacro MidiSetInfo (refNum p)
"Set the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon."
  `(ff-call *midiShare* :word ,refNum :ptr ,p :d0 9))

;;................................................................................: MidiNewFilter
(defmacro MidiNewFilter ()
"Returns a new filter"
  `(ff-call *midiShare* :d0 #x51 :ptr))

;;................................................................................: MidiFreeFilter
(defmacro MidiFreeFilter (f)
"Delete a filter"
  `(ff-call *midiShare* :ptr ,f :d0 #x52))

;;................................................................................: MidiAcceptChan
(defmacro MidiAcceptChan (f c s)
"Change the chan state of a filter"
  `(ff-call *midiShare* :ptr ,f :word ,c :word ,s :d0 #x54 ))

;;................................................................................: MidiAcceptType
(defmacro MidiAcceptType (f c s)
"Change the type state of a filter"
  `(ff-call *midiShare* :ptr ,f :word ,c :word ,s :d0 #x55 ))

;;................................................................................: MidiAcceptPort
(defmacro MidiAcceptPort (f c s)
"Change the port state of a filter"
  `(ff-call *midiShare* :ptr ,f :word ,c :word ,s :d0 #x53 ))

;;................................................................................: MidiIsAcceptedChan
(defmacro MidiIsAcceptedChan (f c)
"Returns the chan state of a filter"
  `(%%word-high-byte (ff-call *midiShare* :ptr ,f :word ,c :d0 #x57 :word)))

;;................................................................................: MidiIsAcceptedType
(defmacro MidiIsAcceptedType (f c)
"Returns the type state of a filter"
  `(%%word-high-byte (ff-call *midiShare* :ptr ,f :word ,c :d0 #x58 :word)))

;;................................................................................: MidiIsAcceptedPort
(defmacro MidiIsAcceptedPort (f c)
"Returns the port state of a filter"
  `(%%word-high-byte (ff-call *midiShare* :ptr ,f :word ,c :d0 #x56 :word)))

;;................................................................................: MidiGetFilter
(defmacro MidiGetFilter (refNum)
"Give a pointer to the input filter record of a MidiShare application. 
 Give NIL if no filter is installed"
  `(ff-call *midiShare* :word ,refNum :d0 10 :ptr))

;;................................................................................: MidiSetFilter
(defmacro MidiSetFilter (refNum p)
"Install an input filter. The argument p is a pointer to a filter record."
  `(ff-call *midiShare* :word ,refNum :ptr ,p :d0 11))

;;................................................................................: MidiGetRcvAlarm
(defmacro MidiGetRcvAlarm (refNum)
"Get the adress of the receive alarm"
  `(ff-call *midiShare* :word ,refNum :d0 #x0C :ptr))

;;................................................................................: MidiSetRcvAlarm
(defmacro MidiSetRcvAlarm (refNum alarm)
"Install a receive alarm"
  `(ff-call *midiShare* :word ,refNum :ptr ,alarm :d0 #x0D))

;;................................................................................: MidiGetApplAlarm
(defmacro MidiGetApplAlarm (refNum)
"Get the adress of the context alarm"
  `(ff-call *midiShare* :word ,refNum :d0 #x0E :ptr))

;;................................................................................: MidiSetApplAlarm
(defmacro MidiSetApplAlarm (refNum alarm)
"Install a context alarm"
  `(ff-call *midiShare* :word ,refNum :ptr ,alarm :d0 #x0F))

;;---------------------------------------------------------------------------------
;;			To Manage MidiShare IAC and Midi Ports
;;---------------------------------------------------------------------------------

;;................................................................................: MidiConnect
(defmacro MidiConnect (src dst state)
"Connect or disconnect two MidiShare applications"
  `(ff-call *midiShare* :word ,src :word ,dst :word ,state :d0 #x10))

;;................................................................................: MidiIsConnected
(defmacro MidiIsConnected (src dst)
"Test if two MidiShare applications are connected"
  `(%%word-high-byte (ff-call *midiShare* :word ,src :word ,dst :d0 #x11 :word)))

;;................................................................................: MidiGetPortState
(defmacro MidiGetPortState (port)
"Give the state : open or closed, of a MidiPort"
  `(%%word-high-byte (ff-call *midiShare* :word ,port :d0 #x12 :word)))

;;................................................................................: MidiSetPortState
(defmacro MidiSetPortState (port state)
"Open or close a MidiPort"
  `(ff-call *midiShare* :word ,port :word ,state :d0 #x13))

;;---------------------------------------------------------------------------------
;;			To Manage MidiShare events
;;---------------------------------------------------------------------------------

;;................................................................................: MidiFreeSpace
(defmacro MidiFreeSpace ()
"Amount of free MidiShare cells"
  `(ff-call *midiShare* :d0 #x14 :long))

;;................................................................................: MidiNewEv
(defmacro MidiNewEv (typeNum)
"Allocate a new MidiEvent"
  `(ff-call *midiShare* :word ,typeNum :d0 #x15 :ptr))

;;................................................................................: MidiCopyEv
(defmacro MidiCopyEv (ev)
"Duplicate a MidiEvent"
  `(ff-call *midiShare* :ptr ,ev :d0 #x16 :ptr))

;;................................................................................: MidiFreeEv
(defmacro MidiFreeEv (ev)
"Free a MidiEvent"
  `(ff-call *midiShare* :ptr ,ev :d0 #x17))


;;................................................................................: MidiSetField
(defmacro MidiSetField (ev field val)
"Set a field of a MidiEvent"
  `(ff-call *midiShare* :ptr ,ev :long ,field :long ,val :d0 #x3A))

;;................................................................................: MidiGetField
(defmacro MidiGetField (ev field)
"Get a field of a MidiEvent"
  `(ff-call *midiShare* :ptr ,ev :long ,field :d0 #x3B :long))

;;................................................................................: MidiAddField
(defmacro MidiAddField (ev val)
"Append a field to a MidiEvent (only for sysex and stream)"
  `(ff-call *midiShare* :ptr ,ev :long ,val :d0 #x1A))

;;................................................................................: MidiCountFields
(defmacro MidiCountFields (ev)
"The number of fields of a MidiEvent"
  `(ff-call *midiShare* :ptr ,ev :d0 #x3C :long))

;;---------------------------------------------------------------------------------
;;			To Manage MidiShare Sequences
;;---------------------------------------------------------------------------------

;;................................................................................: MidiNewSeq
(defmacro MidiNewSeq ()
"Allocate an empty sequence"
  `(ff-call *midiShare* :d0 #x1D :ptr))

;;................................................................................: MidiAddSeq
(defmacro MidiAddSeq (seq ev)
"Add an event to a sequence"
  `(ff-call *midiShare* :ptr ,seq :ptr ,ev :d0 #x1E))

;;................................................................................: MidiFreeSeq
(defmacro MidiFreeSeq (seq)
"Free a sequence and its content"
  `(ff-call *midiShare* :ptr ,seq :d0 #x1F))

;;................................................................................: MidiClearSeq
(defmacro MidiClearSeq (seq)
"Free only the content of a sequence. The sequence become empty"
  `(ff-call *midiShare* :ptr ,seq :d0 #x20))

;;................................................................................: MidiApplySeq
(defmacro MidiApplySeq (seq proc)
"Call a function for every events of a sequence"
  `(ff-call *midiShare* :ptr ,seq :ptr ,proc :d0 #x21))

;;---------------------------------------------------------------------------------
;;				   MidiShare Time
;;---------------------------------------------------------------------------------

;;................................................................................: MidiGetTime
(defmacro MidiGetTime ()
"give the current time"
  `(ff-call *midiShare* :d0 #x22 :long))

;;---------------------------------------------------------------------------------
;;				To Send MidiShare events
;;---------------------------------------------------------------------------------

;;................................................................................: MidiSendIm
(defmacro MidiSendIm (refNum ev)
"send an event now"
  `(ff-call *midiShare* :word ,refNum :ptr ,ev :d0 #x23))

;;................................................................................: MidiSend
(defmacro MidiSend (refNum ev)
"send an event using its own date"
  `(ff-call *midiShare* :word ,refNum :ptr ,ev :d0 #x24))

;;................................................................................: MidiSendAt
(defmacro MidiSendAt (refNum ev date)
"send an event at date <date>"
  `(ff-call *midiShare* :word ,refNum :ptr ,ev :long ,date :d0 #x25))

;;---------------------------------------------------------------------------------
;;                            To Receive MidiShare Events
;;---------------------------------------------------------------------------------

;;................................................................................: MidiCountEvs
(defmacro MidiCountEvs (refNum)
"Give the number of events waiting in the reception fifo"
  `(ff-call *midiShare* :word ,refNum :d0 #x26 :long))

;;................................................................................: MidiGetEv
(defmacro MidiGetEv (refNum)
"Read an event from the reception fifo"
  `(ff-call *midiShare* :word ,refNum :d0 #x27 :ptr))

;;................................................................................: MidiAvailEv
(defmacro MidiAvailEv (refNum)
"Get a pointer to the first event in the reception fifo without removing it"
  `(ff-call *midiShare* :word ,refNum :d0 #x28 :ptr))

;;................................................................................: MidiFlushEvs
(defmacro MidiFlushEvs (refNum)
"Delete all the events waiting in the reception fifo"
  `(ff-call *midiShare* :word ,refNum :d0 #x29))

;;---------------------------------------------------------------------------------
;;                             To access shared data
;;---------------------------------------------------------------------------------

;;................................................................................: MidiReadSync
(defmacro MidiReadSync (adrMem)
"Read and clear a memory address (not-interruptible)"
  `(ff-call *midiShare* :ptr ,adrMem :d0 #x2A :ptr))

;;................................................................................: MidiWriteSync
(defmacro MidiWriteSync (adrMem val)
"write if nil into a memory address (not-interruptible)"
  `(ff-call *midiShare* :ptr ,adrMem :ptr ,val :d0 #x2B :ptr))

;;---------------------------------------------------------------------------------
;;                               Realtime Tasks
;;---------------------------------------------------------------------------------

;;................................................................................: MidiCall
(defmacro MidiCall (proc date refNum arg1 arg2 arg3)
"Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>"
  `(ff-call *midiShare* :ptr ,proc :long ,date :word ,refNum :long ,arg1 :long ,arg2 :long ,arg3 :d0 #x2C))

;;................................................................................: MidiTask
(defmacro MidiTask (proc date refNum arg1 arg2 arg3)
"Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. 
 Return a pointer to the corresponding typeProcess event"
  `(ff-call *midiShare* :ptr ,proc :long ,date :word ,refNum :long ,arg1 :long ,arg2 :long ,arg3 :d0 #x2D :ptr))

;;................................................................................: MidiDTask
(defmacro MidiDTask (proc date refNum arg1 arg2 arg3)
"Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. 
 Return a pointer to the corresponding typeDProcess event"
  `(ff-call *midiShare* :ptr ,proc :long ,date :word ,refNum :long ,arg1 :long ,arg2 :long ,arg3 :d0 #x2E :ptr))

;;................................................................................: MidiForgetTaskHdl
(defmacro MidiForgetTaskHdl (thdl)
"Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  `(ff-call *midiShare* :ptr ,thdl :d0 #x2F))

;;................................................................................: MidiForgetTask
(defmacro MidiForgetTask (ev)
"Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"
  `(without-interrupts 
    (%stack-block ((taskptr 4))
      (%put-ptr taskptr ,ev) (midiforgetTaskHdl taskptr))))

;;................................................................................: MidiCountDTasks
(defmacro MidiCountDTasks (refNum)
"Give the number of typeDProcess events waiting"
  `(ff-call *midiShare* :word ,refNum :d0 #x30 :long))

;;................................................................................: MidiFlushDTasks
(defmacro MidiFlushDTasks (refNum)
"Remove all the typeDProcess events waiting"
  `(ff-call *midiShare* :word ,refNum :d0 #x31))

;;................................................................................: MidiExec1DTask
(defmacro MidiExec1DTask (refNum)
"Call the next typeDProcess waiting"
  `(ff-call *midiShare* :word ,refNum :d0 #x32))

;;---------------------------------------------------------------------------------
;;                        Low Level MidiShare Memory Management
;;---------------------------------------------------------------------------------

;;................................................................................: MidiNewCell
(defmacro MidiNewCell ()
"Allocate a basic Cell"
  `(ff-call *midiShare* :d0 #x33 :ptr))

;;................................................................................: MidiFreeCell
(defmacro MidiFreeCell (cell)
"Delete a basic Cell"
  `(ff-call *midiShare* :ptr ,cell :d0 #x34))

;;................................................................................: MidiTotalSpace
(defmacro MidiTotalSpace ()
"Total amount of Cells"
  `(ff-call *midiShare* :d0 #x35 :long))

;;................................................................................: MidiGrowSpace
(defmacro MidiGrowSpace (n)
"Total amount of Cells"
  `(ff-call *midiShare* :long ,n :d0 #x36 :long))


;;---------------------------------------------------------------------------------
;;                        SMPTE Synchronisation functions
;;---------------------------------------------------------------------------------

;;................................................................................: MidiGetSyncInfo
(defmacro MidiGetSyncInfo (syncInfo)
"Fill syncInfo with current synchronisation informations"
  `(ff-call *midiShare* :ptr ,syncInfo :d0 #x38))

;;................................................................................: MidiSetSyncMode
(defmacro MidiSetSyncMode (mode)
"set the MidiShare synchroniation mode"
  `(ff-call *midiShare* :word ,mode :d0 #x39))

;;................................................................................: MidiGetExtTime
(defmacro MidiGetExtTime ()
"give the current external time"
  `(ff-call *midiShare* :d0 #x3D :long))

;;................................................................................: MidiInt2ExtTime
(defmacro MidiInt2ExtTime (time)
"convert internal time to external time"
  `(ff-call *midiShare* :long ,time :d0 #x3E :long))

;;................................................................................: MidiExt2IntTime
(defmacro MidiExt2IntTime (time)
"convert internal time to external time"
  `(ff-call *midiShare* :long ,time :d0 #x3F :long))

;;................................................................................: MidiTime2Smpte
(defmacro MidiTime2Smpte (time format smpteLocation)
"convert time to Smpte location"
  `(ff-call *midiShare* :long ,time :word ,format :ptr ,smpteLocation :d0 #x40))

;;................................................................................: MidiSmpte2Time
(defmacro MidiSmpte2Time (smpteLocation)
"convert time to Smpte location"
  `(ff-call *midiShare* :ptr ,smpteLocation :d0 #x41 :long))


;;---------------------------------------------------------------------------------
;;                        Drivers functions
;;---------------------------------------------------------------------------------

;;................................................................................: MidiCountDrivers
(defmacro MidiCountDrivers ()
  "number of opened drivers"
  `(%%unsigned-to-signed-word (ff-call *midiShare* :d0 #x46 :word)))

;;................................................................................: MidiGetIndDriver
(defmacro MidiGetIndDriver  (index)
  "Give the reference number of a MidiShare driver from its index, a fixnum"
  `(%%unsigned-to-signed-word (ff-call *midiShare* :word ,index :d0 #x47 :word)))

;;................................................................................: MidiGetDriverInfos
(defmacro MidiGetDriverInfos  (refNum  info)
  "Give information about a driver"
  `(not (eq 0 (%%word-high-byte(ff-call *midiShare* :word ,refNum :ptr ,info :d0 #x48 :word)))))

;;................................................................................: MidiGetIndSlot
(defmacro MidiGetIndSlot  (refNum  index)
  "Give the reference number of a driver slot from its order number."
   `(ff-call *midiShare* :word ,refNum :word ,index  :d0 #x4A :long))

;;................................................................................: MidiGetSlotInfos
 (defmacro MidiGetSlotInfos  (slotRefNum info)
    "Give information about a slot"
    `(not (eq 0 (%%word-high-byte(ff-call *midiShare* :long ,slotRefNum :ptr ,info :d0 #x4C :word)))))

;;................................................................................: MidiConnectSlot
 (defmacro MidiConnectSlot  (port slotRefNum state)
    "Make or remove a connection between a slot and a MidiShare logical port"
    `(ff-call *midiShare* :word ,port :long ,slotRefNum :short  ,state :d0 #x4D))

;;................................................................................: MidiIsSlotConnected
(defmacro MidiIsSlotConnected  (port slotRefNum)
  "Test a connection between a slot and a MidiShare logical port"
  `(not (eq 0 (%%word-high-byte(ff-call *midiShare* :word ,port :long ,slotRefNum :d0 #x4E :word)))))



;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 			To Install and Remove the MidiShare Interface
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

;;---------------------------------------------------------------------------------
;; 	 		MidiShare Startup and Quit Actions
;;---------------------------------------------------------------------------------

;;................................................................................: install-midishare-interface
(defun install-midishare-interface ()
  (setq *midiShare* (%get-ptr (%int-to-ptr #xB8)))
  (unless (midishare) (error "MidiShare not installed")))

;;................................................................................: remove-midishare-interface
(defun remove-midishare-interface ()
  (setq *midiShare* nil))


)   ;; End of MCL interface


;;---------------------------------------------------------------------------------
;; 			    Interface for CMULisp on Linux
;;---------------------------------------------------------------------------------

#+(and linux cmu)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*warn-if-redefine* nil))
    (load-foreign "/usr/lib/libMidiShare.so")
    (use-package "ALIEN"                    )
    (use-package "C-CALL"                   )
    )
  )

#+(and linux cmu)

;;---------------------------------------------------------------------------------
;;
;; 				Utilities                                         
;;
;;---------------------------------------------------------------------------------

(progn

;; define a general pointer type
(def-alien-type ptr  (* t))

;; Test if p is a null pointer
                                                                      
(defun nullptrp (p)
  (if (typep p '(alien (* t))) 
    (zerop (system:sap-int (alien-sap p)))
    (zerop (system:sap-int p))))

;; Returns a numm pointer

(defun nullptr () (system:int-sap 0))


;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;                                                                                 
;; 				MidiShare Data Structures                           
;;                                                                                  
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------


;; Extension record for typeSysEx events

(def-alien-type nil
  (struct TMidiSEX
	  (link  (*(struct TMidiSEX)) )
	  (data  (array char 12)      )
  )
)


;; Extension record for typePrivate, typeProcess and typeDProcess events

(def-alien-type nil
  (struct TMidiST
	  (ptr1 ptr )
	  (ptr2 ptr )
	  (ptr3 ptr )
	  (ptr4 ptr )
  )
)
	  

;;---------------------------------------------------------------------------------
;; Common Record for all MidiShare events                                        
;;---------------------------------------------------------------------------------

;; Possible types for the last field of the structure
                                                      
(def-alien-type nil                                   
  (struct PitchVelDur                                
	  (pitch char  )                              
	  (vel   char  )                             
	  (dur   short )))                         
                                                     
(def-alien-type nil                                   
  (struct datas                                       
	  (data0 char)                               
	  (data1 char)                                
	  (data2 char)                               
	  (data3 char) ))                             
                                                      
(def-alien-type T_info long)                          
(def-alien-type T_linkSE (*(struct TMidiSEX)))        
(def-alien-type T_linkST (*(struct TMidiST) ))       
                                                      
(def-alien-type nil                                   
   (union evData                                      
	  (note (struct pitchVelDur))                
	  (data (struct datas      ))                 
	  (info T_info              )                
	  (linkSE T_linkSE          )               
	  (linkST T_linkST          )))            
                                                     
                                                      
;; The last field of a TMidiEv is either :  
;;                                                    
;;     - a note (with a pitch, a velocite and a duration) 
;;     - a 4 byte field (4 au total)            
;;     - a fiels info  (4 bytes)                                   
;;     - a link to a TMidiSEX                     
;;     - a link to a TMidiST                      
;;                                                    

(def-alien-type nil
  (struct TMidiEv
	  (link    (*(struct TMidiEv))  )
	  (date    long                 )
	  (evtype  char                 )
	  (ref     char                 )
	  (port    char                 )
	  (chan    char                 )
	  (data  (union evData)         )
   )
)


;;---------------------------------------------------------------------------------
;; Record for a MidiShare Sequence                                                 
;;---------------------------------------------------------------------------------

(def-alien-type nil
  (struct TMidiSeq
	  (first   (*(struct TMidiEv)) )
	  (last    (*(struct TMidiEv)) )
	  (undef1  ptr                 )
	  (undef2  ptr                 )
  )
)

;;---------------------------------------------------------------------------------
;; Record for MidiShare SMPTE synchronisation informations                          
;;---------------------------------------------------------------------------------

(def-alien-type nil
  (struct TSyncInfo
	  (time        long   )
	  (reenter     long   )
	  (syncMode    short  )
	  (syncLocked  char   )
	  (syncPort    char   )
	  (syncStart   long   )
	  (syncStop    long   )
	  (syncOffset  long   )
	  (syncSpeed   long   )
	  (SyncBreaks  long   )
	  (syncFormat  short  )
  )
)

;;---------------------------------------------------------------------------------
;; Record for MidiShare SMPTE locations                                              
;;---------------------------------------------------------------------------------

(def-alien-type nil
  (struct TSmpteLocation
	  (format   short )
	  (hours    short )
	  (minutes  short )
	  (seconds  short )
	  (frames   short )
	  (fracs    short )
  )
)

;;--------------------------------------------------------------------------------
;; Record for MidiShare Filters                                                   
;;--------------------------------------------------------------------------------

(def-alien-type nil
  (struct TFilter
	  (port    (array char 32) )
	  (evType  (array char 32) )
	  (channel (array char 2)  )
	  (unused  (array char 2)  )
  )
)

;;--------------------------------------------------------------------------------
;; pointers on alien data structures                                               
;;--------------------------------------------------------------------------------

(def-alien-type MidiSEXPtr     ( *(struct TMidiSEX) )      ) 
(def-alien-type MidiSTPtr      ( *(struct TMidiST))        ) 
(def-alien-type MidiEvPtr      ( *(struct TMidiEv))        )
(def-alien-type MidiSeqPtr     ( *(struct TMidiSeq))       )
(def-alien-type SyncInfoPtr    ( *(struct TSyncInfo))      )
(def-alien-type SmpteLocPtr    ( *(struct TSmpteLocation)) )
(def-alien-type MidiFilterPtr  ( *(struct TFilter))        )


;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;                                                                                  
;; 		Macros for accessing MidiShare Events data structures              
;;                                                                                 
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------


;;---------------------------------------------------------------------------------
;;                      Macros common to every type of event
;;---------------------------------------------------------------------------------

;;................................................................................: link
;;                                                                                                                                          
(defmacro link (e &optional (d nil d?))
"read or set the link of an event"
  (if d?
    `(midiSetLink ,e ,d);  `(setf (slot ,e 'link) ,d )                 
    `(midiGetLink ,e   );  `(slot ,e 'link           )
  )
)

;;................................................................................: date
;;                                                                                                                                       
(defmacro date (e &optional d)
  "read or set the date of an event"
  (if d
      `(MidiSetDate ,e ,d); `(setf (slot ,e 'date) ,d )
      `(MidiGetDate ,e   ); `(slot ,e 'date           )                
  )
)

;;................................................................................: type
;;                                                                                                                                          
(defmacro type (e &optional v)
"read or set the type of an event. Be careful in 
 modifying the type of an event"
  (if v
      `(MidiSetType ,e ,v); `(setf (slot ,e 'evType) ,v )
      `(MidiGetType ,e   ); `(slot ,e 'evType           )              
  )
)

;;................................................................................: ref
;;                                                                                                                                          
(defmacro ref (e &optional v)
"read or set the reference number of an event"
  (if v
      `(MidiSetRefNum ,e ,v); `(setf (slot ,e 'ref) ,v)
      `(MidiGetRefNum e    ); `(slot ,e 'ref          )                
  )
)

;;................................................................................: port
;;                                                                                                                                             
(defmacro port (e &optional v)
"read or set the port number of an event"
  (if v
      `(MidiSetPort ,e ,v); `(setf (slot ,e 'port) ,v)
      `(MidiGetPort ,e   ); `(slot ,e 'port          )                 
   )
)

;;................................................................................: chan
;;                                                                                                                                           
(defmacro chan (e &optional v)
"read or set the chan number of an event"
  (if v
      `(MidiSetChan ,e ,v); `(setf (slot ,e 'chan) ,v)
      `(MidiGetChan ,e   ); `(slot ,e 'chan          )                  
  )
)

;;................................................................................: field
;;                                                                                                                                           
(defmacro field (e &optional f v)
"give the number of fields or read or set a particular field of an event"
  (if f
    (if v
      `(midisetfield ,e ,f ,v)
      `(midigetfield ,e ,f))
    `(midicountfields ,e)))

;;................................................................................: fieldsList
;;                                                                                                                   
(defun fieldsList (e &optional (n 4))
"collect all the fields of an event into a list"
  (let (l)
    (dotimes (i (min n (midicountfields e)))
      (push (midigetfield e i) l))
    (nreverse l)))


;;---------------------------------------------------------------------------------
;;                         Specific to typeNote events
;;---------------------------------------------------------------------------------

;;................................................................................: pitch
;;                                                                                                                                            
(defmacro pitch (e &optional v)
"read or set the pitch of an event"
  (if v
      `(midisetfield ,e 0 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'pitch) ,v)
      `(midigetfield ,e 0   ); `(slot (slot (slot ,e 'data) 'note) 'pitch          )  
  )
)

;;................................................................................: vel
;;                                                                                                                                         
(defmacro vel (e &optional v)
"read or set the velocity of an event"
  (if v
      `(midisetfield ,e 1 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'vel) ,v)
      `(midigetfield ,e 1   ); `(slot (slot (slot ,e 'data) 'note) 'vel          )    
  )
)

;;................................................................................: dur
;;                                                                                                                                             
(defmacro dur (e &optional v)
"read or set the duration of an event"
  (if v
      `(midisetfield ,e 2 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'dur) ,v)
      `(midigetfield ,e 2   ); `(slot (slot (slot ,e 'data) 'note) 'dur          )    
  )
)


;;---------------------------------------------------------------------------------
;;                        Specific to other types of events
;;---------------------------------------------------------------------------------

;;................................................................................: linkSE
;;                                                                                               
(defmacro linkSE (e &optional (d nil d?))
"read or set the link of an SEXevent "
  (if d?
      `(setf (slot (slot ,e 'data) 'linkSE) ,d)
      `(slot (slot ,e 'data) 'linkSE          )
  )
)

;;................................................................................: linkST
;;                                                                                                
(defmacro linkST (e &optional (d nil d?))
 "read or set the link of an STevent "
  (if d?
      `(setf (slot (slot ,e 'data) 'linkST) ,d)
      `(slot (slot ,e 'data) 'linkST          )
  )
)


;;................................................................................: kpress
;;                                                                                               
(defmacro kpress (e &optional v)
  (if v
      `(midisetfield ,e 1 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'vel) ,v)
      `(midigetfield ,e 1   ); `(slot (slot (slot ,e 'data) 'note) 'vel          )  
  )
)


;;................................................................................: ctrl
;;                                                                                               
(defmacro ctrl (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: param
;;                                                                                             
(defmacro param (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: num
;;                                                                                              
(defmacro num (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: prefix
;;                                                                                               
(defmacro prefix (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: tempo
;;                                                                                                
(defmacro tempo (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: seconds
;;                                                                                               
(defmacro seconds (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: subframes
;;                                                                                                
(defmacro subframes (e &optional v)
  (if v
    `(midisetfield ,e 1 ,v)
    `(midigetfield ,e 1)))


;;................................................................................: val
;;                                                                                               
(defmacro val (e &optional v)
  (if v
    `(midisetfield ,e 1 ,v)
    `(midigetfield ,e 1)))


;;................................................................................: pgm
;;                                                                                               
(defmacro pgm (e &optional v)
  (if v
      `(midisetfield ,e 0 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'pitch) ,v)
      `(midigetfield ,e 0   ); `(slot (slot (slot ,e 'data) 'note) 'pitch          )  
  )
)


;;................................................................................: bend
;;                                                                                                
(defmacro bend (e &optional v)
  "read or set the bend value of an event"
  (if v
      `(multiple-value-bind (ms7b ls7b) (floor (+ ,v 8192) 128)
         (midisetfield ,e 0 ls7b); (setf (slot (slot (slot ,e 'data) 'note) 'pitch) ls7b)
         (midisetfield ,e 1 ms7b); (setf (slot (slot (slot ,e 'data) 'note) 'vel  ) ms7b)
       )
       `(- (+ (midigetfield ,e 0) (* 128 (midigetfield ,e 1))) 8192)                                                
       ; `(- (+ (slot (slot (slot ,e 'data) 'note) 'pitch) (* 128 (slot (slot (slot ,e 'data) 'note) 'vel))) 8192)
  )
)


;;................................................................................: clk
;;                                                                                                
(defmacro clk (e &optional v)
  (if v
      `(multiple-value-bind (ms7b ls7b) (floor (round (/ ,v 6)) 128)
         (midisetfield ,e 0 ls7b); (setf (slot (slot (slot ,e 'data) 'note) 'pitch) ls7b)
         (midisetfield ,e 1 ms7b); (setf (slot (slot (slot ,e 'data) 'note) 'vel  ) ms7b)
       )
      `(* 6 (+ (midigetfield ,e 0) (*128 (midigetfield ,e 1))))                                              
      ; `(* 6 (+ (slot (slot (slot ,e 'data) 'note) 'pitch) (* 128 (slot (slot (slot ,e 'data) 'note) 'vel))))
  )
)


;;................................................................................: song
;;                                                                                                
(defmacro song (e &optional v)
  (if v
      `(midisetfield ,e 0 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'pitch) ,v)
      `(midigetfield ,e 0   ); `(slot (slot (slot ,e 'data) 'note) 'pitch          )   
  )
)


;;................................................................................: fields
;;                                                                                               
(defmacro fields (e &optional v)
  (if v
    `(let ((e ,e)) (mapc #'(lambda (f) (midiaddfield e f)) ,v))
    `(let (l (e ,e))  (dotimes (i (midicountfields e)) (push (midigetfield e i) l)) (nreverse l)) ))


;;................................................................................: text
;;                                                                                                
(defmacro text (e &optional s)
  (if s
    `(fields ,e (map 'list #'char-code ,s))
    `(map 'string #'character (fields ,e)) ))


;;................................................................................: fmsg
;;                                                                                               
(defmacro fmsg (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'pitch) ,v)
    `(midigetfield ,e 0   ); `(slot (slot (slot ,e 'data) 'note) 'pitch          )   
  )
)

;;................................................................................: fcount
;;                                                                                               
(defmacro fcount (e &optional v)
  (if v
    `(midisetfield ,e 1 ,v); `(setf (slot (slot (slot ,e 'data) 'note) 'vel) ,v)
    `(midigetfield ,e 1   ); `(slot (slot (slot ,e 'data) 'note) 'vel)                
  )
)

;;................................................................................: tsnum
;;                                                                                                
(defmacro tsnum (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))


;;................................................................................: tsdenom
;;                                                                                               
(defmacro tsdenom (e &optional v)
  (if v
    `(midisetfield ,e 1 ,v)
    `(midigetfield ,e 1)))


;;................................................................................: tsclick
;;                                                                                               
(defmacro tsclick (e &optional v)
  (if v
    `(midisetfield ,e 2 ,v)
    `(midigetfield ,e 2)))


;;................................................................................: tsquarter
;;                                                                                              
(defmacro tsquarter (e &optional v)
  (if v
    `(midisetfield ,e 3 ,v)
    `(midigetfield ,e 3)))

;;................................................................................: alteration
;;                                                                                                
(defmacro alteration (e &optional v)
  (if v
    `(midisetfield ,e 0 ,v)
    `(midigetfield ,e 0)))

;;................................................................................: minor-scale
;;                                                                                                
(defmacro minor-scale (e &optional v)
  (if v
    `(midisetfield ,e 1 (if ,v 1 0))
    `(= 1 (midigetfield ,e 1))))

;;................................................................................: info
;;                                                                                                                                        
(defmacro info (e &optional d)
"read or set the info of an event"
  (if d
    `(setf (slot (slot ,e 'data) 'info) ,d)
    `(slot (slot ,e 'data) 'info          )
  )
)



;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;
;; 		Macros for accessing MidiShare Sequences data structures
;;
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

;;................................................................................: firstEv
;;                                                                                                                                             
(defmacro firstEv (s &optional (e nil e?))
  "read or set the first event of a sequence"
  (if e?
      `(midiSetFirstEv ,s ,e); `(setf (slot ,s 'first) ,e)
      `(midiGetFirstEv ,s   ); `(slot ,s 'first          )          
  )
)

;;................................................................................: lastEv
;;                                                                                                                                          
(defmacro lastEv (s &optional (e nil e?))
"read or set the last event of a sequence"
  (if e?
      `(midiSetLastEv ,s ,e); `(setf (slot ,s 'last) ,e)
      `(midiGetLastEv ,s   ); `(slot ,s 'last          )           
  )
)


;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------
;;                                                                                    
;; 				MidiShare Entry Points                                
;;                                                                                   
;;---------------------------------------------------------------------------------
;;---------------------------------------------------------------------------------

;; Interface description for a MidiShare PROCEDURE 
;; with short parameter
;; (def-alien-routine "My_Procedure" void
;;       (parameter1 short))
;;
;; Interface description for a MidiShare FUNCTION
;; with 2 short parameters and a short result
;; (def-alien-routine "My_Function" short
;;       (parameter1 short) (parameter2 short))


;; Entry point of MidiShare (setup at boot time by the "MidiShare" init)

(defvar *midiShare*)

;;---------------------------------------------------------------------------------
;;			To Know about MidiShare and Active Sessions
;;---------------------------------------------------------------------------------

;;................................................................................: MidiShare 
;;                                                                                                                                         
(declaim (inline MidiShare))
(def-alien-routine "MidiShare" char 
  "returns true if MidiShare is installed")

;;................................................................................: MidiGetVersion
;;                                                                                                                                        
(declaim (inline MidiGetVersion))
(def-alien-routine "MidiGetVersion" short
"Give MidiShare version as a fixnum. For example 131 as result, means : version 1.31")

;;................................................................................: MidiCountAppls
;;                                                                                                                                         
(declaim (inline MidiCountAppls))
(def-alien-routine "MidiCountAppls" short
"Give the number of MidiShare applications currently opened")

;;................................................................................: MidiGetIndAppl
;;                                                                                                                                           
(declaim (inline MidiGetIndAppl))
(def-alien-routine "MidiGetIndAppl" short
  (index short)
"Give the reference number of a MidiShare application from its index, a fixnum
 between 1 and (MidiCountAppls)")

;;................................................................................: MidiGetNamedAppl
;;                                                                                                                                            
(declaim (inline MidiGetNamedAppl))
(def-alien-routine "MidiGetNamedAppl" short
  (name c-string)
"Give the reference number of a MidiShare application from its name")


;;---------------------------------------------------------------------------------
;;			To Open and Close a MidiShare session
;;---------------------------------------------------------------------------------

;;................................................................................: MidiOpen
;;                                                                                                                                          
(declaim (inline MidiOpen))
(def-alien-routine "MidiOpen" short
  (name c-string)
"Open a new MidiShare application, with name name. Give a unique reference number.")

;;................................................................................: MidiClose
;;                                                                                                                                         
(declaim (inline MidiClose))
(def-alien-routine "MidiClose" void
  (refNum short)
"Close an opened MidiShare application from its reference number")



;;---------------------------------------------------------------------------------
;;			To Configure a MidiShare session
;;---------------------------------------------------------------------------------

;;................................................................................: MidiGetName
;;                                                                                                                                        
(declaim (inline MidiGetName))
(def-alien-routine "MidiGetName" c-string
  (refNum short)
"Give the name of a MidiShare application from its reference number")

;;................................................................................: MidiSetName
;;                                                                                                                                   
(declaim (inline MidiSetName))
(def-alien-routine "MidiSetName" void
  (refNum short) (name c-string)                                       
"Change the name of a MidiShare application")

;;................................................................................: MidiGetInfo
;;                                                                                                                                             
(declaim (inline MidiGetInfo))
(def-alien-routine "MidiGetInfo" (* t)
  (refNum short)
"Give the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon.")

;;................................................................................: MidiSetInfo
;;                                                                                                                                                
(declaim (inline MidiSetInfo))
(def-alien-routine "MidiSetInfo" void
  (refNum short) (p (* t))
"Set the 32-bits user defined content of the info field of a MidiShare application. 
 Analogous to window's refcon.")


;;................................................................................: MidiNewFilter
;;                                                                                                                                          
(declaim (inline MidiNewFilter))
(def-alien-routine "MidiNewFilter" MidiFilterPtr
"Returns a new filter")

;;................................................................................: MidiFreeFilter
;;                                                                                                                                         
(declaim (inline MidiFreeFilter))
(def-alien-routine "MidiFreeFilter" void
  (f MidiFilterPtr)
"Delete a filter")

;;................................................................................: MidiAcceptChan
(declaim (inline MidiAcceptChan))
(def-alien-routine "MidiAcceptChan" void
  (f MidiFilterPtr) (c short) (s short)
"Change the chan state of a filter")

;;................................................................................: MidiAcceptType
(declaim (inline MidiAcceptType))
(def-alien-routine "MidiAcceptType" void
  (f MidiFilterPtr) (c short) (s short)
"Change the type state of a filter")

;;................................................................................: MidiAcceptPort
(declaim (inline MidiAcceptPort))
(def-alien-routine "MidiAcceptPort" void
  (f MidiFilterPtr) (c short) (s short)
"Change the port state of a filter")

;;................................................................................: MidiIsAcceptedChan
(declaim (inline MidiIsAcceptedChan))
(def-alien-routine "MidiIsAcceptedChan" short
  (f MidiFilterPtr) (c short)
"Returns the chan state of a filter")

;;................................................................................: MidiIsAcceptedType
(declaim (inline MidiIsAcceptedType))
(def-alien-routine "MidiIsAcceptedType" short
  (f MidiFilterPtr) (c short)
"Returns the type state of a filter")

;;................................................................................: MidiIsAcceptesPort
(declaim (inline MidiIsAcceptedPort))
(def-alien-routine "MidiIsAcceptedPort" short
  (f MidiFilterPtr) (c short)
"Returns the port state of a filter")

;;................................................................................: MidiGetFilter
;;                                                                                                              
(declaim (inline MidiGetFilter))
(def-alien-routine "MidiGetFilter" MidiFilterPtr
  (refNum short)
"Give a pointer to the input filter record of a MidiShare application. 
 Give NIL if no filter is installed")

;;................................................................................: MidiSetFilter
;;                                                                                                                                        
(declaim (inline MidiSetFilter))
(def-alien-routine "MidiSetFilter" void
  (refNum short) ( p MidiFilterPtr)
"Install an input filter. The argument p is a pointer to a filter record.")

;;................................................................................: MidiGetRcvAlarm
;;                                                                                              
(declaim (inline MidiGetRcvAlarm))
(def-alien-routine "MidiGetRcvAlarm" ptr
  (refNum short)
"Get the adress of the receive alarm")

;;................................................................................: MidiSetRcvAlarm
;;                                                                                              
(declaim (inline MidiSetRcvAlarm))
(def-alien-routine "MidiSetRcvAlarm" void
  (refNum short) (alarm ptr )
"Install a receive alarm")

;;................................................................................: MidiGetApplAlarm
;;                                                                                            
(declaim (inline MidiGetApplAlarm))
(def-alien-routine "MidiGetApplAlarm" ptr
  (refNum short)
"Get the adress of the context alarm")

;;................................................................................: MidiSetApplAlarm
;;                                                                                              
(declaim (inline MidiSetApplAlarm))
(def-alien-routine "MidiSetApplAlarm" void
  (refNum short) (alarm ptr)
"Install a context alarm")



;;---------------------------------------------------------------------------------
;;			To Manage MidiShare IAC and Midi Ports
;;---------------------------------------------------------------------------------

;;................................................................................: MidiConnect
;;                                                                                                                                          
(declaim (inline MidiConnect))
(def-alien-routine "MidiConnect" void
  (src short) (dst short) (state char)
"Connect or disconnect two MidiShare applications")

;;................................................................................: MidiIsConnected
;;                                                                                                                                          
(declaim (inline MidiIsConnected))
(def-alien-routine "MidiIsConnected" char
  (src short) (dst short)
"Test if two MidiShare applications are connected")

;;................................................................................: MidiGetPortState
;;                                                                                                                               
(declaim (inline MidiGetPortState))
(def-alien-routine "MidiGetPortState" char
  (port short)
"Give the state : open or closed, of a MidiPort")

;;................................................................................: MidiSetPortState
;;                                                                                                                               
(declaim (inline MidiSetPortState))
(def-alien-routine "MidiSetPortState" void
  (port short) (state char)
"Open or close a MidiPort")


;;---------------------------------------------------------------------------------
;;			To Manage MidiShare events
;;---------------------------------------------------------------------------------

;;................................................................................: MidiFreeSpace
;;                                                                                                                                              
(declaim (inline MidiFreeSpace))
(def-alien-routine "MidiFreeSpace" long
"Amount of free MidiShare cells")

;;................................................................................: MidiNewEv
;;                                                                                                                                        
(declaim (inline MidiNewEv))
(def-alien-routine "MidiNewEv" MidiEvPtr
  (typeNum short)
"Allocate a new MidiEvent")

;;................................................................................: MidiCopyEv
;;                                                                                                                                          
(declaim (inline MidiCopyEv))
(def-alien-routine "MidiCopyEv" MidiEvPtr
  (ev MidiEvPtr)
"Duplicate a MidiEvent")

;;................................................................................: MidiFreeEv
;;                                                                                                                                         
(declaim (inline MidiFreeEv))
(def-alien-routine "MidiFreeEv" void
  (ev MidiEvPtr)
"Free a MidiEvent")

;;................................................................................: MidiSetField
;;                                                                                                                                        
(declaim (inline MidiSetField))
(def-alien-routine "MidiSetField" void
  (ev MidiEvPtr) (field long) (val long)
"Set a field of a MidiEvent")

;;................................................................................: MidiGetField
;;                                                                                                                                      
(declaim (inline MidiGetField))
(def-alien-routine "MidiGetField" long
  (ev MidiEvPtr) (field long)
"Get a field of a MidiEvent")

;;................................................................................: MidiAddField
;;                                                                                                                                        
(declaim (inline MidiAddField))
(def-alien-routine "MidiAddField" void
  (ev MidiEvPtr) (val long)
"Append a field to a MidiEvent (only for sysex and stream)")

;;................................................................................: MidiCountFields
;;                                                                                                                                           
(declaim (inline MidiCountFields))
(def-alien-routine "MidiCountFields" long
  (ev MidiEvPtr)
"The number of fields of a MidiEvent")

;;................................................................................: MidiGetDate
(declaim (inline MidiGetDate))
(def-alien-routine "MidiGetDate" long
  (ev MidiEvPtr))

;;................................................................................: MidiSetDate
(declaim (inline MidiSetDate))
(def-alien-routine "MidiSetDate" void
  (ev MidiEvPtr) (date long))
  
;;................................................................................: MidiGetLink
(declaim (inline MidiGetLink))
(def-alien-routine "MidiGetLink" MidiEvPtr
  (ev MidiEvPtr))

;;................................................................................: MidiSetLink
(declaim (inline MidiSetLink))
(def-alien-routine "MidiSetLink" void
  (ev MidiEvPtr) (linkEv MidiEvPtr))

;;................................................................................: MidiGetRefnum
(declaim (inline MidiGetRefNum))
(def-alien-routine "MidiGetRefNum" short
  (ev MidiEvPtr))

;;................................................................................: MidiSetRefnum
(declaim (inline MidiSetRefNum))
(def-alien-routine "MidiSetRefNum" void
  (ev MidiEvPtr) (date short))

;;................................................................................: MidiGetType
(declaim (inline MidiGetType))
(def-alien-routine "MidiGetType" short
  (ev MidiEvPtr))

;;................................................................................: MidiSetType
(declaim (inline MidiSetType))
(def-alien-routine "MidiSetType" void
  (ev MidiEvPtr) (date short))


;;................................................................................: MidiGetChan
(declaim (inline MidiGetChan))
(def-alien-routine "MidiGetChan" short
  (ev MidiEvPtr))

;;................................................................................: MidiSetChan
(declaim (inline MidiSetChan))
(def-alien-routine "MidiSetChan" void
  (ev MidiEvPtr) (date short))


;;................................................................................: MidiGetPort
(declaim (inline MidiGetPort))
(def-alien-routine "MidiGetPort" short
  (ev MidiEvPtr))

;;................................................................................: MidiSetPort
(declaim (inline MidiSetPort))
(def-alien-routine "MidiSetPort" void
  (ev MidiEvPtr) (date short))



;;---------------------------------------------------------------------------------
;;			To Manage MidiShare Sequences
;;---------------------------------------------------------------------------------


;;................................................................................: MidiGetFirstEv
(declaim (inline MidiGetFirstEv))
(def-alien-routine "MidiGetFirstEv" MidiEvPtr
  (seq MidiSeqPtr))

;;................................................................................: MidiSetFirstEv
(declaim (inline MidiSetFirstEv))
(def-alien-routine "MidiSetFirstEv" void
  (seq MidiSeqPtr) (ev MidiEvPtr))


;;................................................................................: MidiGetLastEv
(declaim (inline MidiGetLastEv))
(def-alien-routine "MidiGetLastEv" MidiEvPtr
  (seq MidiSeqPtr))

;;................................................................................: MidiSetLastEv
(declaim (inline MidiSetLastEv))
(def-alien-routine "MidiSetLastEv" void
  (seq MidiSeqPtr) (ev MidiEvPtr))


;;................................................................................: MidiNewSeq
;;                                                                                                                                      
(declaim (inline MidiNewSeq))
(def-alien-routine "MidiNewSeq" MidiSeqPtr       
"Allocate an empty sequence")

;;................................................................................: MidiAddSeq
;;                                                                                                                                     
(declaim (inline MidiAddSeq))
(def-alien-routine "MidiAddSeq" void
  (seq MidiSeqPtr ) (ev MidiEvPtr)
"Add an event to a sequence")

;;................................................................................: MidiFreeSeq
;;                                                                                                                                      
(declaim (inline MidiFreeSeq))
(def-alien-routine "MidiFreeSeq" void
  (seq MidiSeqPtr )
"Free a sequence and its content")
;  `(ff-call *midiShare* :ptr ,seq :d0 #x1F))

;;................................................................................: MidiClearSeq
;;                                                                                                                                            
(declaim (inline MidiClearSeq))
(def-alien-routine "MidiClearSeq" void
  (seq MidiSeqPtr )
"Free only the content of a sequence. The sequence become empty")
;  `(ff-call *midiShare* :ptr ,seq :d0 #x20))

;;................................................................................: MidiApplySeq
;;                                                                                                                                               
(declaim (inline MidiApplySeq))
(def-alien-routine "MidiApplySeq" void
  (seq MidiSeqPtr ) (proc ptr)
"Call a function for every events of a sequence")


;;---------------------------------------------------------------------------------
;;				   MidiShare Time
;;---------------------------------------------------------------------------------

;;................................................................................: MidiGetTime
;;                                                                                                                                          
(declaim (inline MidiGetTime))
(def-alien-routine "MidiGetTime" long
"give the current time")

;;---------------------------------------------------------------------------------
;;				To Send MidiShare events
;;---------------------------------------------------------------------------------

;;................................................................................: MidiSendIm
;;                                                                                                                                       
(def-alien-routine "MidiSendIm" void
  (refNum short) (ev MidiEvPtr)
"send an event now")

;;................................................................................: MidiSend
;;                                                                                                                                           
(declaim (inline MidiSend))
(def-alien-routine "MidiSend" void
  (refNum short) (ev MidiEvPtr)
"send an event using its own date")

;;................................................................................: MidiSendAt
;;                                                                                                                            
(declaim (inline MidiSendAt))
(def-alien-routine "MidiSendAt" void
  (refNum short) (ev MidiEvPtr) (date long)
"send an event at date <date>")

;;---------------------------------------------------------------------------------
;;                            To Receive MidiShare Events
;;---------------------------------------------------------------------------------

;;................................................................................: MidiCountEvs
;;                                                                                                                                             
(declaim (inline MidiCountEvs))
(def-alien-routine "MidiCountEvs" long
  (refNum short)
"Give the number of events waiting in the reception fifo")

;;................................................................................: MidiGetEv
;;                                                                                                                                              
(declaim (inline MidiGetEv))
(def-alien-routine "MidiGetEv" MidiEvPtr
  (refNum short)
"Read an event from the reception fifo")

;;................................................................................: MidiAvailEv
;;                                                                                                                                          
(declaim (inline MidiAvailEv))
(def-alien-routine "MidiAvailEv" MidiEvPtr
  (refNum short)
"Get a pointer to the first event in the reception fifo without removing it")

;;................................................................................: MidiFlushEvs
;;                                                                                                                                             
(declaim (inline MidiFlushEvs))
(def-alien-routine "MidiFlushEvs" void
  (refNum short)
"Delete all the events waiting in the reception fifo")

;;---------------------------------------------------------------------------------
;;                             To access shared data
;;---------------------------------------------------------------------------------

;;................................................................................: MidiReadSync
;;                                                                                                                                       
(declaim (inline MidiRedSync))
(def-alien-routine "MidiReadSync" (* t)
  (adrMem (* t) )
"Read and clear a memory address (not-interruptible)")

;;................................................................................: MidiWriteSync
;;                                                                                                                                   
(declaim (inline MidiWriteSync))
(def-alien-routine "MidiWriteSync" (* t)
  (adrMem (* t) ) (val (* t) )
"write if nil into a memory address (not-interruptible)")

;;---------------------------------------------------------------------------------
;;                               Realtime Tasks
;;---------------------------------------------------------------------------------

;;................................................................................: MidiCall
;;                                                                                                                                            
(declaim (inline MidiCall))
(def-alien-routine "MidiCall" void
  (proc ptr) (date long) (refNum short) (arg1 long) (arg2 long) (arg3 long)
"Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>")

;;................................................................................: MidiTask
;;                                                                                                                                            
(declaim (inline MidiTask))
(def-alien-routine "MidiTask" ptr
  (proc ptr) (date long) (refNum short) (arg1 long) (arg2 long) (arg3 long)
"Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. 
 Return a pointer to the corresponding typeProcess event")

;;................................................................................: MidiDTask
;;                                                                                                                                                 
(declaim (inline MidiDTask))
(def-alien-routine "MidiDTask" ptr
  (proc ptr) (date long) (refNum short) (arg1 long) (arg2 long) (arg3 long)
"Call the routine <proc> at date <date> with arguments <arg1> <arg2> <arg3>. 
 Return a pointer to the corresponding typeDProcess event")


;;................................................................................: MidiForgetTaskH * a VOIR POUR LES MODIFS *
;;(defmacro MidiForgetTaskHdl (thdl)                                                                                                                             *
;;"Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"                                                             *
;;  `(ff-call *midiShare* :ptr ,thdl :d0 #x2F))                                                                                                                  *
;;................................................................................: MidiForgetTask                                                               * 
;;                                                                                                                                                               *    
;;(defmacro MidiForgetTask (ev)                                                                                                                                  *
;;"Forget a previously scheduled typeProcess or typeDProcess event created by MidiTask or MidiDTask"                                                             *
;;  `(without-interrupts                                                                                                                                         *
;;    (%stack-block ((taskptr 4))                                                                                                                                *
;;      (%setf-macptr taskptr ,ev) (midiforgetTaskHdl taskptr))))                                                                                                *
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;*


;;................................................................................: MidiCountDTasks
;;                                                                                                                                            
(declaim (inline MidiCountDTasks))
(def-alien-routine "MidiCountDTasks" long
  (refNum short)
"Give the number of typeDProcess events waiting")

;;................................................................................: MidiFlushDTasks
;;                                                                                                                                                
(declaim (inline MidiFlushDTasks))
(def-alien-routine "MidiFlushDTasks" void
  (refNum short)
"Remove all the typeDProcess events waiting")

;;................................................................................: MidiExec1DTask
;;                                                                                                                                              
(declaim (inline MidiExec1DTask))
(def-alien-routine "MidiExec1DTask" void
  (refNum short)
"Call the next typeDProcess waiting")



;;---------------------------------------------------------------------------------
;;                        Low Level MidiShare Memory Management
;;---------------------------------------------------------------------------------

;;................................................................................: MidiNewCell
;;                                                                                                                                           
(declaim (inline MidiNewCell))
(def-alien-routine "MidiNewCell" MidiEvPtr
"Allocate a basic Cell")

;;................................................................................: MidiFreeCell
;;                                                                                                                                           
(declaim (inline MidiFreeCell))
(def-alien-routine "MidiFreeCell" void
  (cell MidiEvPtr)
"Delete a basic Cell")

;;................................................................................: MidiTotalSpace
;;                                                                                                                                          
(declaim (inline MidiTotalSpace))
(def-alien-routine "MidiTotalSpace" long
"Total amount of Cells")

;;................................................................................: MidiGrowSpace
;;                                                                                                                                              
(declaim (inline MidiGrowSpace))
(def-alien-routine "MidiGrowSpace" long
  (n long)
"Total amount of Cells")


;;---------------------------------------------------------------------------------
;;                        SMPTE Synchronisation functions
;;---------------------------------------------------------------------------------

;;................................................................................: MidiGetSyncInfo
;;                                                                                              
(declaim (inline MidiGetSyncInfo))
(def-alien-routine "MidiGetSyncInfo" void
  (syncInfo SyncInfoPtr   )
"Fill syncInfo with current synchronisation informations")

;;................................................................................: MidiSetSyncMode
;;                                                                                               
(declaim (inline MidiSetSyncMode))
(def-alien-routine "MidiSetSyncMode" void
  (mode short)
"set the MidiShare synchroniation mode")

;;................................................................................: MidiGetExtTime
;;                                                                                                                                   
(declaim (inline MidiGetExtTime))
(def-alien-routine "MidiGetExtTime" long
"give the current external time")

;;................................................................................: MidiInt2ExtTime
;;                                                                                                                                      
(declaim (inline MidiInt2ExtTime))
(def-alien-routine "MidiInt2ExtTime" long
  (time long)
"convert internal time to external time")

;;................................................................................: MidiExt2IntTime
;;                                                                                                                                     
(declaim (inline MidiExt2IntTime))
(def-alien-routine "MidiExt2IntTime" long
  (time long)
"convert internal time to external time")

;;................................................................................: MidiTime2Smpte
;;                                                                                                                                                
(declaim (inline MidiTime2Smpte))
(def-alien-routine "MidiTime2Smpte" void
  (time long) (format short) (smpteLocation SmpteLocPtr )
"convert time to Smpte location")

;;................................................................................: MidiSmpte2Time
;;                                                                                                                                              
(declaim (inline MidiSmpte2Time))
(def-alien-routine "MidiSmpte2Time" long
  (smpteLocation SmpteLocPtr )
"convert time to Smpte location")

;;................................................................................: install-midishare-interface

(defun install-midishare-interface ()
  (unless (midishare) (error "MidiShare not installed")))

;;................................................................................: remove-midishare-interface

(defun remove-midishare-interface ())


) ;; End of CMULisp interface



;;---------------------------------------------------------------------------------
;; 	 			**Evaluate this**
;;---------------------------------------------------------------------------------;; ******MOVED TO MAKE-DM2 /AF*********
;(eval-when (:load-toplevel :execute)
;  (add-startup-action #'install-midishare-interface)
;  (add-quit-action #'remove-midishare-interface)
;  (install-midishare-interface))
  
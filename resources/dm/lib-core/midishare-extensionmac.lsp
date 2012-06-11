;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;
;;
;;                                MidiShare-Extension.lisp
;;
;;                                     © 1991, GRAME.
;;
;;
;;
;;
;;
;;  Ce fichier reprend et etend l'ensemble des fonctions de MidiShare.
;;
;;
;;  CONVENTION D'ECRITURE : 
;;  De même que dans MidiShare toutes les fonctions ont un nom de la forme "midixxyy",
;;  toutes les fonctions definis ici auront un nom de la forme "midi-xx-yy".
;;
;;
;;  HISTORIQUE :
;;  31-08-91, Première version issue de MidiShare-appls.Lisp. -Yo-
;;  03-12-94, Adaptation au nouveau mode de chargement et suppression
;;	      du package MidiShare.
;;  13-04-01  Ajout du type PortPrefix SL
;;  13-06-01  Nettoyage pour rendre le code multi-platorm :
;;            - %null-ptr-p remplace par une fonction generique nullptrp
;;	      - typeText remplace par typeTextual
;;	      - Utilisation de MidiNewFilter, MidiFreeFilter, MidiAcceptXXX
;;  19-06-01   Changement du fonctionnement des fonctions de connection et etat des filtres 
;;             pour rendre le code multi-platorm
;;  25-06-01   Macptr remplace par t pour faciliter le portage Linux
;;             Suppression de la methode midi-new-filter (double emploi avec la fonction midi-new-filter)
;;
;;
;;/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\
;;\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

;;               <<<<<< CE FICHIER NECESSITE "MIDISHARE-INTERFACE.LISP" >>>>>>


;;                        Macros d'acces aux evenements MidiShare
;;========================================================================================


;;                               Les primitives MidiShare
;;========================================================================================
;; Alors que dans le fichier "MidiShare-interface.lisp" toutes les primitives MidiShare
;; sont implementees sous forme de macros, ici elles sont reprisent sous la forme de fonctions
;; ou, quand cela est possible, sous la forme de methodes CLOS. Elles sont en outre "securisees".


(defmacro midi-check-ptr (e)
  `(if (nullptrp ,e) (error "MidiShare run out of memory")))


;;...................................................................: midi-get-version
(defun midi-get-version ()
  (MidiGetVersion))


;;...................................................................: midi-count-appls
(defun midi-count-appls ()
  (MidiCountAppls))


;;...................................................................: midi-get-ind-appl
(defmethod midi-get-ind-appl ((index integer))
  (MidiGetIndAppl index))


;;...................................................................: midi-get-named-appl
(defmethod midi-get-named-appl ((name string))
  (MidiGetNamedAppl name))


;;...................................................................: midi-open
(defmethod midi-open ((name string))
  (let ((ref (MidiOpen name)))
    (if (< ref 1)
      (error "MidiShare error code : ~S" ref)
      ref)))


;;...................................................................: midi-open
(defmethod midi-close ((refnum integer))
  (MidiClose refnum))


;;...................................................................: midi-get-name
(defmethod midi-get-name ((refnum integer))
  (MidiGetName refnum))


;;...................................................................: midi-set-name
(defmethod midi-set-name ((refnum integer) (name string))
  (MidiSetName refnum name)
  name)


;;...................................................................: midi-get-info
(defmethod midi-get-info ((refnum integer))
  (MidiGetInfo refnum))


;;...................................................................: midi-set-info
(defmethod midi-set-info ((refnum integer) (info t))
  (MidiSetInfo refnum info)
  info)


;;...................................................................: midi-free-filter
(defmethod midi-free-filter ((filter t))
  (midifreefilter filter))


;;...................................................................: midi-accept-chan
(defmethod midi-accept-chan ((filter t) (chan integer) (state t))
  (midiacceptchan filter chan (if state -1 0)))


;;...................................................................: midi-accept-type
(defmethod midi-accept-type ((filter t) (type integer) (state t))
  (midiaccepttype filter type (if state -1 0)))


;;...................................................................: midi-accept-port
(defmethod midi-accept-port ((filter t) (port integer) (state t))
  (midiacceptport filter port (if state -1 0)))


;;...................................................................: midi-is-accepted-chan
(defmethod midi-is-accepted-chan ((filter t) (chan integer))
  (not (= 0 (midiisacceptedchan filter chan))))


;;...................................................................: midi-is-accepted-type
(defmethod midi-is-accepted-type ((filter t) (type integer))
  (not (= 0 (midiisacceptedtype filter type))))
  

;;...................................................................: midi-is-accepted-port
(defmethod midi-is-accepted-port ((filter t) (port integer))
  (not (= 0 (midiisacceptedport filter port))))


;;...................................................................: midi-get-filter
(defmethod midi-get-filter ((refnum integer))
  (midigetfilter refnum))


;;...................................................................: midi-set-filter
(defmethod midi-set-filter ((refnum integer) (filter t))
  (midisetfilter refnum filter)
  filter)


;;...................................................................: midi-get-rcv-alarm
(defmethod midi-get-rcv-alarm ((refnum integer))
  (MidiGetRcvAlarm refnum))


;;...................................................................: midi-set-rcv-alarm
(defmethod midi-set-rcv-alarm ((refnum integer) (foo t))
  (MidiSetRcvAlarm refnum foo)
  foo)


;;...................................................................: midi-get-appl-alarm
(defmethod midi-get-appl-alarm ((refnum integer))
  (MidiGetApplAlarm refnum))


;;...................................................................: midi-set-appl-alarm
(defmethod midi-set-appl-alarm ((refnum integer) (foo t))
  (MidiSetApplAlarm refnum foo)
  foo)


;;...................................................................: midi-connect
(defmethod midi-connect ((src integer) (dst integer) (state t))
  (midiconnect src dst (if state -1 0))
  state)


;;...................................................................: midi-is-connected
(defmethod midi-is-connected ((src integer) (dst integer))
  (not (= 0 (MidiIsConnected src dst))))


;;...................................................................: midi-get-port-state
(defmethod midi-get-port-state ((port integer))
  (not (= 0 (MidiGetPortState port))))


;;...................................................................: midi-set-port-state
(defmethod midi-set-port-state ((port integer) (state t))
  (MidiSetPortState port (if state -1 0)))


;;...................................................................: midi-free-space
(defun midi-free-space ()
  (midifreespace))


;;...................................................................: midi-new-ev
(defmethod midi-new-ev ((evtype integer))
  (let ((e (midinewev evtype)))
    (when (nullptrp e) 
      (cerror "Try to increase MidiShare memory" "MidiShare run out of memory")
      (midi-Grow-space 1000)
      (setq e (midinewev evtype))
      (if (nullptrp e) (error "Failed to increase MidiShare memory")))
    e))


;;...................................................................: midi-copy-ev
(defmethod midi-copy-ev ((e1 t))
  (let ((e2 (midicopyev e1)))
    (when (nullptrp e2) 
      (cerror "Try to increase MidiShare memory" "MidiShare run out of memory")
      (midi-Grow-space 1000)
      (setq e2 (midicopyev e1))
      (if (nullptrp e2) (error "Failed to increase MidiShare memory")))    
    e2))


;;...................................................................: midi-free-ev
(defmethod midi-free-ev ((e t))
  (midifreeev e))


;;...................................................................: midi-set-field
(defmethod midi-set-field ((e t) (field integer) (val t))
  (midisetfield e field val))


;;...................................................................: midi-get-field
(defmethod midi-get-field ((e t) (field integer))
  (midigetfield e field))


;;...................................................................: midi-add-field
(defmethod midi-add-field ((e t) (val t))
  (midiaddfield e val))


;;...................................................................: midi-count-fields
(defmethod midi-count-fields ((e t))
  (midicountfields e))


;;...................................................................: midi-new-seq
(defun midi-new-seq ()
  (let ((seq (MidiNewSeq)))
    (when (nullptrp seq) 
      (cerror "try to increase MidiShare memory" "MidiShare run out of memory")
      (midi-Grow-space 1000)
      (setq seq (MidiNewSeq))
      (if (nullptrp seq) (error "Failed to increase MidiShare memory")))    
    seq))


;;...................................................................: midi-add-seq
(defmethod midi-add-seq ((seq t) (ev t))
  (MidiAddSeq seq ev))


;;...................................................................: midi-free-seq
(defmethod midi-free-seq ((seq t))
  (MidiFreeSeq seq))


;;...................................................................: midi-clear-seq
(defmethod midi-clear-seq ((seq t))
  (MidiClearSeq seq))


;;...................................................................: midi-apply-seq
(defmethod midi-apply-seq ((seq t) (foo t))
  (MidiApplySeq seq foo))


;;...................................................................: midi-get-time
(defun midi-get-time ()
  (MidiGetTime))


;;...................................................................: now
(defun now (&optional (offset 0))
  (+ offset (MidiGetTime)))

;;...................................................................: midi-send-im
(defmethod midi-send-im ((refnum integer) (ev t))
  (midisendim refnum ev))


;;...................................................................: midi-send
(defmethod midi-send ((refnum integer) (e t))
  (midisend refnum e))


;;...................................................................: midi-send-at
(defmethod midi-send-at ((refnum integer) (ev t) (date integer))
  (midisendat refnum ev date))


;;...................................................................: midi-count-evs
(defmethod midi-count-evs ((refnum integer) &key sel date pos dur len)
  ; ajout des keyword, pour être compatible avec la methode des scores
  (declare (ignore sel date pos dur len))
  (midicountevs refnum))


;;...................................................................: midi-get-ev
(defmethod midi-get-ev ((refnum integer) &key sel date pos len dur)
  ; ajout des keyword, pour être compatible avec la methode des scores
  (declare (ignore sel date pos len dur))
  (MidiGetEv refnum))


;;...................................................................: midi-avail-ev
(defmethod midi-avail-ev ((refnum integer))
  (MidiAvailEv refnum))


;;...................................................................: midi-flush-evs
(defmethod midi-flush-evs ((refnum integer))
  (MidiFlushEvs refnum))


;;...................................................................: midi-read-sync
(defmethod midi-read-sync ((adrMem t))
  (MidiReadSync adrMem))


;;...................................................................: midi-write-sync
(defmethod midi-write-sync ((adrMem t) (val t))
  (MidiWriteSync adrMem val))


;;...................................................................: midi-call
(defmethod midi-call ((proc t) (date integer) (refnum integer) &rest largs)
  (MidiCall proc date refnum (first largs) (second largs) (third largs)))


;;...................................................................: midi-task
(defmethod midi-task ((proc t) (date integer) (refnum integer) &rest largs)
  (MidiTask proc date refnum (first largs) (second largs) (third largs)))


;;...................................................................: midi-dtask
(defmethod midi-dtask ((proc t) (date integer) (refnum integer) &rest largs)
  (MidiDTask proc date refnum (first largs) (second largs) (third largs)))


;;...................................................................: midi-forget-task
(defmethod midi-forget-task ((task t))
  (MidiForgetTask task))


;;...................................................................: midi-count-dtask
(defmethod midi-count-dtask ((refnum integer))
  (MidiCountDTasks refnum))


;;...................................................................: midi-flush-dtask
(defmethod midi-flush-dtask ((refnum integer))
  (MidiFlushDTasks refnum))


;;...................................................................: midi-exec-1-dtask
(defmethod midi-exec-1-dtask ((refnum integer))
  (MidiExec1DTask refnum))


;;...................................................................: midi-new-cell
(defun midi-new-cell ()
  (let ((e (MidiNewCell)))
    (midi-check-ptr e)
    e))


;;...................................................................: midi-free-cell
(defmethod midi-free-cell ((e t))
  (MidiFreeCell e))


;;...................................................................: midi-total-space
(defun midi-total-space ()
  (MidiTotalSpace))


;;...................................................................: midi-grow-space
(defmethod midi-grow-space ((amount integer))
  (MidiGrowSpace amount))


;;...................................................................: midi-get-sync-info
(defmethod midi-get-sync-info ((syncInfo t))
  (MidiGetSyncInfo syncInfo))


;;...................................................................: midi-set-sync-mode
(defmethod midi-set-sync-mode ((mode integer))
  (MidiSetSyncMode mode))


;;...................................................................: midi-get-ext-time
(defun midi-get-ext-time ()
  (MidiGetExtTime))


;;...................................................................: midi-int-2-ext-time
(defmethod midi-int-2-ext-time ((time integer))
  (MidiInt2ExtTime time))


;;...................................................................: midi-ext-2-int-time
(defmethod midi-ext-2-int-time ((time integer))
  (MidiExt2IntTime time))


;;...................................................................: midi-time-2-smpte
(defmethod midi-time-2-smpte ((time integer) (format integer) (smpteLocation t))
  (MidiTime2Smpte time format smpteLocation))


;;...................................................................: midi-smpte-2-time
(defmethod midi-smpte-2-time ((smpteLocation t))
  (MidiSmpte2Time smpteLocation))



;;                                  Gestion des evenements MidiShare
;;========================================================================================


;;...................................................................: midi-list-fields
(defmethod midi-list-fields ((e t) &optional (n 4))
  (if (or (and (>= (type e) typeTextual) (<= (type e) typeCuePoint)) (= (type e) typeSpecific))
    (text e)
    (let (l)
      (dotimes (i (min n (midicountfields e)))
        (push (midigetfield e i) l))
      (nreverse l))))


;;...................................................................: midi-string-date
(defun midi-string-date (d)
  (let (hh mn ss ms)
    (multiple-value-setq (ss ms) (floor d 1000))
    (multiple-value-setq (mn ss) (floor ss 60))
    (multiple-value-setq (hh mn) (floor mn 60))
    (format nil "~2,'0d:~2,'0d:~2,'0d.~3,'0d" hh mn ss ms)))


;;...................................................................: midi-date-string
(defun midi-date-string (s)
  (let ((d 0) (v 0)(n 0))
    (dotimes (i (length s))
      (cond ((setq n (digit-char-p (char s i)))
             (setq v (+ (* v 10) n)))
            ((eq (char s i) #\:)
             (setq d (* (+ d v) 60))
             (setq v 0))
            ((eq (char s i) #\.)
             (setq d (* (+ d v) 1000))
             (setq v 0))))
    (+ d v)))


;;...................................................................: midi-string-type
(defun midi-string-type (n)
  "convert a MidiShare event type to its name, a string."
  (let* ((lName '((0 . "Note") (1 . "KeyOn") (2 . "KeyOff") (3 . "KeyPress")
                  (4 . "CtrlChange") (5 . "ProgChange") (6 . "ChanPress") (7 . "PitchBend")
                  (8 . "SongPos") (9 . "SongSel") (10 . "Clock") (11 . "Start")
                  (12 . "Cont") (13 . "Stop") (14 . "Tune") (15 . "ActiveSens")
                  (16 . "Reset") (17 . "SysEx") (18 . "Stream") 
                  (128 . "Process") (129 . "DProcess") (130 . "QFrame")
                  (131 . "Ctrl14b") (132 . "NonRegParam") (133 . "RegParam")
                  (134 . "SeqNum") (135 . "Text:") (136 . "Copyright:")
                  (137 . "SeqName:") (138 . "InstrName:") (139 . "Lyric:")
                  (140 . "Marker:") (141 . "CuePoint:") (142 . "ChanPrefix")
                  (143 . "EndTrack") (144 . "Tempo") (145 . "SMPTEOffset")
                  (146 . "TimeSign") (147 . "KeySign") (148 . "Specific:") (149 . "PortPrefix")
                  (255 . "Dead")))
         (l (assoc n lName)) )
    (cond (l 
           (cdr l))
          ((< n typeProcess)
           "Private")
          (t
           "Undefined") )))


;;...................................................................: midi-string-ev
(defmethod midi-string-ev ((e t) &optional (f 4))
  (unless (nullptrp e)
    (list 
    (format nil "~A ~3D/~4A ~A ~:a" 
                (midi-string-date (date e)) 
                (port e) (chan e) 
                (midi-string-type (type e)) 
                (midi-list-fields e f)))))



;;                          Creation des differents types d'evenements
;;========================================================================================


;;...................................................................: note
(defun note (&key (date 0)(pitch 60)(vel 80)(dur 125)(chan 0)(port 0) &aux e)
  "create a note event"
  (setq e (midi-new-ev typeNote))
  (date e (if (stringp date) (midi-date-string date) date))
  (pitch e pitch)
  (vel e vel)
  (dur e dur)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: key-on
(defun key-on (&key (date 0)(pitch 60)(vel 80)(chan 0)(port 0) &aux e)
  "create a key on event"
  (setq e (midi-new-ev typeKeyOn))
  (date e (if (stringp date) (midi-date-string date) date))
  (pitch e pitch)
  (vel e vel)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: key-off
(defun key-off (&key (date 0)(pitch 60)(vel 80)(chan 0)(port 0) &aux e)
  "create a key off event"
  (setq e (midi-new-ev typeKeyOff))
  (date e (if (stringp date) (midi-date-string date) date))
  (pitch e pitch)
  (vel e vel)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: key-press
(defun key-press (&key (date 0)(pitch 60)(kpress 80)(chan 0)(port 0) &aux e)
  "create a key pressure event"
  (setq e (midi-new-ev typekeypress))
  (date e (if (stringp date) (midi-date-string date) date))
  (pitch e pitch)
  (kpress e kpress)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: ctrl-change
(defun ctrl-change (&key (date 0)(ctrl 7)(val 127)(chan 0)(port 0) &aux e)
  "create a control change event"
  (setq e (midi-new-ev typeCtrlChange))
  (date e (if (stringp date) (midi-date-string date) date))
  (ctrl e ctrl)
  (val e val)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: ctrl14b
(defun ctrl14b (&key (date 0)(ctrl 7)(val 127)(chan 0)(port 0) &aux e)
  "create a control change event"
  (setq e (midi-new-ev typeCtrl14b))
  (date e (if (stringp date) (midi-date-string date) date))
  (ctrl e ctrl)
  (val e val)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: non registred parameter
(defun non-reg-param (&key (date 0)(param 0)(val 0)(chan 0)(port 0) &aux e)
  "create a non registred parameter event"
  (setq e (midi-new-ev typeNonRegParam))
  (date e (if (stringp date) (midi-date-string date) date))
  (param e param)
  (val e val)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: registred parameter
(defun reg-param (&key (date 0)(param 0)(val 0)(chan 0)(port 0) &aux e)
  "create a registred parameter event"
  (setq e (midi-new-ev typeRegParam))
  (date e (if (stringp date) (midi-date-string date) date))
  (param e param)
  (val e val)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: sequence number
(defun seq-num (&key (date 0)(num 0)(chan 0)(port 0) &aux e)
  "create a sequence number event"
  (setq e (midi-new-ev typeSeqNum))
  (date e (if (stringp date) (midi-date-string date) date))
  (num e num)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: text event
(defun text-event (&key (date 0)(text "untitled")(chan 0)(port 0) &aux e)
  "create a text event"
  (setq e (midi-new-ev typeTextual))
  (date e (if (stringp date) (midi-date-string date) date))
  (text e text)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: copyright event
(defun copyright (&key (date 0)(text "untitled")(chan 0)(port 0) &aux e)
  "create a copyright event"
  (setq e (midi-new-ev typeCopyright))
  (date e (if (stringp date) (midi-date-string date) date))
  (text e text)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: seqname event
(defun seq-name (&key (date 0)(text "untitled")(chan 0)(port 0) &aux e)
  "create a seqname event"
  (setq e (midi-new-ev typeseqname))
  (date e (if (stringp date) (midi-date-string date) date))
  (text e text)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: instrname event
(defun instr-name (&key (date 0)(text "untitled")(chan 0)(port 0) &aux e)
  "create a instrname event"
  (setq e (midi-new-ev typeinstrname))
  (date e (if (stringp date) (midi-date-string date) date))
  (text e text)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: lyric event
(defun lyric (&key (date 0)(text "untitled")(chan 0)(port 0) &aux e)
  "create a lyric event"
  (setq e (midi-new-ev typelyric))
  (date e (if (stringp date) (midi-date-string date) date))
  (text e text)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: marker event
(defun marker (&key (date 0)(text "untitled")(chan 0)(port 0) &aux e)
  "create a marker event"
  (setq e (midi-new-ev typemarker))
  (date e (if (stringp date) (midi-date-string date) date))
  (text e text)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: cue Point event
(defun cue-point (&key (date 0)(text "untitled")(chan 0)(port 0) &aux e)
  "create a cue Point event"
  (setq e (midi-new-ev typeCuePoint))
  (date e (if (stringp date) (midi-date-string date) date))
  (text e text)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: specific event
(defun specific (&key (date 0)(fields ())(chan 0)(port 0) &aux e)
  "create a specific event"
  (setq e (midi-new-ev typeSpecific))
  (date e (if (stringp date) (midi-date-string date) date))
  (fields e fields)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: channel prefix
(defun chan-prefix (&key (date 0)(prefix 0)(chan 0)(port 0) &aux e)
  "create a channel prefix event"
  (setq e (midi-new-ev typeChanPrefix))
  (date e (if (stringp date) (midi-date-string date) date))
  (prefix e prefix)
  (chan e chan)
  (port e port)
  e)

;;...................................................................: port prefix
(defun port-prefix (&key (date 0)(prefix 0)(chan 0)(port 0) &aux e)
  "create a port prefix event"
  (setq e (midi-new-ev typePortPrefix))
  (date e (if (stringp date) (midi-date-string date) date))
  (prefix e prefix)
  (chan e chan)
  (port e port)
  e)



;;...................................................................: end of track
(defun end-track (&key (date 0)(chan 0)(port 0) &aux e)
  "create an end of track event"
  (setq e (midi-new-ev typeEndTrack))
  (date e (if (stringp date) (midi-date-string date) date))
  (chan e chan)
  (port e port)
  e)


;;...................................................................: tempo-change
(defun tempo-change (&key (date 0)(tempo 0)(chan 0)(port 0) &aux e)
  "create a tempo change event"
  (setq e (midi-new-ev typeTempo))
  (date e (if (stringp date) (midi-date-string date) date))
  (tempo e tempo)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: smpte-offset
(defun smpte-offset (&key (date 0) (seconds 0) (subframes 0) (chan 0)(port 0) &aux e)
  "create an smpte offset event"
  (setq e (midi-new-ev typeSmpteOffset))
  (date e (if (stringp date) (midi-date-string date) date))
  (seconds e seconds)
  (subframes e subframes)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: time-signature
(defun time-Sign (&key (date 0) (tsnum 0) (tsdenom 0) (tsclick 0)(tsquarter 0)(port 0) &aux e)
  "create a time signature event"
  (setq e (midi-new-ev typeTimeSign))
  (date e (if (stringp date) (midi-date-string date) date))
  (tsnum e tsnum)
  (tsdenom e tsdenom)
  (tsclick e tsclick)
  (tsquarter e tsquarter)
  (port e port)
  e)


;;...................................................................: key-signature
(defun key-Sign (&key (date 0) (alteration 0) (minor-scale nil) (port 0) &aux e)
  "create a key signature event"
  (setq e (midi-new-ev typeKeySign))
  (date e (if (stringp date) (midi-date-string date) date))
  (alteration e alteration)
  (minor-scale e minor-scale)
  (port e port)
  e)



;;...................................................................: prog-change
(defun prog-change (&key (date 0)(pgm 0)(chan 0)(port 0) &aux e)
  "create a program change event"
  (setq e (midi-new-ev typeProgChange))
  (date e (if (stringp date) (midi-date-string date) date))
  (pgm e pgm)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: chan-press
(defun chan-press (&key (date 0)(press 80)(chan 0)(port 0) &aux e)
"create a channel pressure event"
  (setq e (midi-new-ev typeChanPress))
  (date e (if (stringp date) (midi-date-string date) date))
  (pitch e press)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: pitch-bend
(defun pitch-bend (&key (date 0)(bend 0)(chan 0)(port 0) &aux e)
  "create a pitch bender event. The argument bend a value between -8192..+8191"
  (setq e (midi-new-ev typePitchWheel))
  (date e (if (stringp date) (midi-date-string date) date))
  (bend e bend)
  (chan e chan)
  (port e port)
  e)


;;...................................................................: song-pos
(defun song-pos (&key (date 0)(clk 0)(port 0) &aux e)
  "create a song position pointer event. ClkNum is IN MIDI CLOCK and divided automaticaly by 6"
  (setq e (midi-new-ev typeSongPos))
  (date e (if (stringp date) (midi-date-string date) date))
  (clk e clk)
  (port e port)
  e)


;;...................................................................: song-sel
(defun song-sel (&key (date 0)(song 0)(port 0) &aux e)
  "create a song select event"
  (setq e (midi-new-ev typeSongSel))
  (date e (if (stringp date) (midi-date-string date) date))
  (song e song)
  (port e port)
  e)


;;...................................................................: clock
(defun clock (&key (date 0)(port 0) &aux e)
  "create a clock event"
  (setq e (midi-new-ev typeClock))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  e)


;;...................................................................: start
(defun start (&key (date 0)(port 0) &aux e)
  "create a start event"
  (setq e (midi-new-ev typeStart))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  e)


;;...................................................................: stop
(defun stop (&key (date 0)(port 0) &aux e)
  "create a stop event"
  (setq e (midi-new-ev typeStop))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  e)


;;...................................................................: cont
(defun cont (&key (date 0)(port 0) &aux e)
  "create a continue event"
  (setq e (midi-new-ev typeContinue))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  e)


;;...................................................................: tune
(defun tune (&key (date 0)(port 0) &aux e)
  "create a tune event"
  (setq e (midi-new-ev typeTune))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  e)


;;...................................................................: active-sens
(defun active-sens (&key (date 0)(port 0) &aux e)
  "create a active sensing event"
  (setq e (midi-new-ev typeActiveSens))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  e)


;;...................................................................: reset
(defun reset (&key (date 0)(port 0) &aux e)
  "create a reset event"
  (setq e (midi-new-ev typeReset))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  e)


;;...................................................................: sys-ex
(defun sys-ex (&key (date 0)(fields ())(chan 0)(port 0) &aux e)
  "create a system exclusive event from a list of values. 
 Leading F0 and tailing F7 MUST NOT be included in the list. 
 They are provided by MidiShare when the event is actually sended to output"
  (setq e (midi-new-ev typeSysEx))
  (date e (if (stringp date) (midi-date-string date) date))
  (chan e chan)
  (port e port)
  (fields e fields)
  e)


;;...................................................................: stream
(defun stream (&key (date 0)(fields ())(port 0) &aux e)
"create an arbitrary stream of bytes (both status and data) 
 sended by MidiShare without any processing."
  (setq e (midi-new-ev typeStream))
  (date e (if (stringp date) (midi-date-string date) date))
  (port e port)
  (fields e fields)
  e)


;;...................................................................: qframe
(defun qframe (&key (date 0)(fmsg 0) (fcount 0)(port 0) &aux e)
"create a quarter frame event"
  (setq e (midi-new-ev typeQFrame))
  (date e (if (stringp date) (midi-date-string date) date))
  (fmsg e fmsg)
  (fcount e fcount)
  (port e port)
  e)


;;...................................................................: private
(defun private (&key (date 0) (subtype 0) 
                     (field0 0) (field1 0) 
                     (field2 0) (field3 0) &aux e)
  "create a private event. subtype, an integer between 0..108 is a subtype for the private event."
  (setq e (midi-new-ev (+ typePrivate subtype)))
  (date e (if (stringp date) (midi-date-string date) date))
  (Field e 0 field0)
  (Field e 1 field1)
  (Field e 2 field2)
  (Field e 3 field3)
  e)



;;                              Creation, modification de filtres
;;========================================================================================


;;...................................................................: midi-new-filter
(defun midi-new-filter (&key chan port type)
  (let ((f (MidiNewFilter)))
    (cond ((eq chan t) (dotimes (i 16) (MidiAcceptChan f i -1)))
          ((numberp chan) (MidiAcceptChan f chan -1))
          (t (dolist (i chan) (MidiAcceptChan f i -1))))
    (cond ((eq type t) (dotimes (i 256) (MidiAcceptType f i -1)))
          ((numberp type) (MidiAcceptType f type -1))
          (t (dolist (i type) (MidiAcceptType f i -1))))
    (cond ((eq port t) (dotimes (i 256) (MidiAcceptPort f i -1)))
          ((numberp port) (MidiAcceptPort f port -1))
          (t (dolist (i port) (MidiAcceptPort f i -1)))) 
    f))


;;...................................................................: midi-free-filter
(defmethod midi-free-filter ((f t))
  (MidiFreeFilter f))	


;;...................................................................: midi-modify-filter
(defmethod midi-modify-filter ((f t) &key accept chan port type)
  (let ((state (if accept -1 0)))
    (unless (nullptrp f)
      (cond ((eq chan t) (dotimes (i 16) (MidiAcceptChan f i state)))
            ((numberp chan) (MidiAcceptChan f chan state))
            (t (dolist (i chan) (MidiAcceptChan f i state))))
      (cond ((eq type t) (dotimes (i 256) (MidiAcceptType f i state)))
            ((numberp type) (MidiAcceptType f type state))
            (t (dolist (i type) (MidiAcceptType f i state))))
      (cond ((eq port t) (dotimes (i 256) (MidiAcceptPort f i state)))
            ((numberp port) (MidiAcceptPort f port state))
            (t (dolist (i port) (MidiAcceptPort f i state)))))))



;;                              Quelques predicats sur les evenements
;;========================================================================================
(defun is-note-p (e) (eq (type e) typenote))

(defun is-key-p (e) (<= (type e) typeKeyPress))

(defun is-keystart-p (e) (and (eq (type e) typekeyon) (> (vel e) 0)))

(defun is-keyend-p (e) (or (eq (type e) typekeyoff) 
                           (and (eq (type e) typekeyon) (= (vel e) 0))))


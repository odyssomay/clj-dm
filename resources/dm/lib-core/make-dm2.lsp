;;;-*-Mode: LISP; Package: DM -*-
;;
;; **********************************
;;   loading and setting the system 
;; **********************************

(in-package :dm)

;; =============================
;; global dm system variables
;; =============================

(defvar *demo-version* nil)
(setq *demo-version* nil) ; *********** set t if making standalone *******

(defvar *coda-demo-version* nil)
(setq *coda-demo-version* nil)

(defvar *notesenses-version* nil)
(setq *notesenses-version* nil)

(defvar *dm-def-files* nil)

;; =============================
;; global dm variables
;; =============================

(defvar *env-settings*) ;default environment variables object
(defvar *active-score*)  ;default score


;; =============================
;; global mcl system variables
;; =============================

#+:mcl
(progn
  (setq *load-verbose* t)
  (setq *verbose-eval-selection* t)
  (setq *save-definitions* (if *demo-version* nil t))
  (setq *save-local-symbols* (if *demo-version* nil t))
  (setq *fasl-save-local-symbols* (if *demo-version* nil t))
  (setq  *print-case* :downcase)
  )

;; ---------------------------------------
;;   SET PLATFORM DEPENDANT VARIABLES
;; ---------------------------------------
;;
#+:MCL (defvar *.lisp-pathname* (pathname ".lsp"))
;#+:MCL (defvar *.fasl-pathname* (pathname ".fasl"))
   
#+:MCL (egc nil) ;turn on the Ephemeral garbage collection
   
#+:mswindows (defvar *.fasl-pathname* ".fasl")
#+:mswindows (defvar *.lisp-pathname* ".lsp")


;; =============================
;;   LOAD UTILITIES AND STUFFS
;; =============================
;;
#| flyttat
(load "dm:lib-core;platform-dependencies.lsp")
;(set-platform-dependant-variables-PD)   ; before load-utilities-PD !
(load "dm:lib-core;read-and-compile.lsp")
(load "dm:lib-core;dm-objects.lsp")
(setq *env-settings* (make-instance 'environment-settings))  ;*********** save/load status

#+:mswindows ;this will start the file dialogs in the same folder as the program
(when *demo-version*
   (set-dm-var 'lib-directory nil)       
   (set-dm-var 'rules-directory nil)       
   (set-dm-var 'rule-sets-directory nil)       
   (set-dm-var 'music-directory nil)       
   (set-dm-var 'log-directory nil)       
   (set-dm-var 'current-music-directory nil)       
   (set-dm-var 'current-rule-sets-directory nil)
   )
|#

;; ==================
;;  DEFINE SYSTEM
;; ==================
;;
;; for many of the old Mac "lib" files, an ACL Win95 version is provided when
;; dependant on the platform because of graphic, interface and midishare commands.
;;


(setf *dm-def-files*
      '(    ; the list of all files in dm
        "dm:lib-core;platform-dependencies"
        "dm:lib-core;read-and-compile"   ;tveksam om den behovs - mojligen MCL
        "dm:lib-core;dm-objects"
        "dm:lib-core;infixmath"
        "dm:lib-core;basicmacros"
        "dm:lib-core;scoreobjects"
        "dm:lib-core;shapeobjects"
        "dm:lib-core;syntobjects"
        "dm:lib-core;rule-dialog-common"
        "dm:lib-core;rulemacros"
        "dm:lib-core;parallelrulemacros"
        ;"envelopes"
        "dm:lib-core;initconvert"
        "dm:lib-core;musicio"
        "dm:lib-core;utilityrules"
        "dm:rules;frules1"
        "dm:rules;frules2"
        "dm:rules;synconmel"
        ;"synconnote"
        "dm:rules;intonation"
        "dm:rules;punctuation"
        "dm:rules;swing"
        "dm:rules;phrasearch"
        "dm:rules;finalritard"
        "dm:rules;noise"
        "dm:rules;articulation"
        "dm:rules;violinvibrato"
        "dm:rules;accent-rule-ebrp"
        ;"piano"
        ;"accents" 
        
        "dm:lib-core;rule-groups"
        "dm:lib-core;playlist"
        ;"musici/oplaylist"
        ;"musicdialog"
        ;"prefdialog"
        "dm:lib-core;exportindividualruledata"
        "dm:lib-core;save-pdm-score"
        "dm:lib-core;midifileoutput" 
        "dm:lib-core;midifileinput" 
        ))

#+:lispworks
(setf *dm-def-files* (append *dm-def-files* 
  '(
    "dm:lib-core;midibasic-lw"
    "dm:lib-gui;musicmenus-lw"
    )))

#+(and :mswindows :allegro)
(setf *dm-def-files* (append *dm-def-files* 
  '(
    "dm:lib-gui;musicmenus-win"
    "dm:lib-gui;ruledialog-win"
    "dm:lib-gui;musicdialog-win"
    "dm:lib-gui;drawprop-win-part1"
    "dm:lib-gui;drawprop-win-part2"
    "dm:lib-gui;drawbasic-win"
    "dm:lib-gui;drawpolynotes-win"
    "dm:lib-gui;drawselectvars-win"
    "dm:lib-gui;utilitydialog-win"  

    ;"dm:lib-gui;drawnotes-win"
    ;"dm:midishare;mshare32"        ; load midishare for win 95
    ;"dm:midishare;mshext32"        ; load midishare for win 95
    ;"dm:midishare;develop;midishare1632lisp;lisp;msh32thk"   ; load midishare for win 95
    ;"dm:midishare;develop;midishare1632lisp;lisp;mshext32"        ; load midishare for win 95
    ;"dm:lib-core;midishare-toolbox-win"

   "dm:lib-core;midibasic-midishare"
   ; "dm:lib-core;midifileoutput" 
   ; "dm:lib-core;midifileinput" 
    "dm:lib-gui;midifile-input-dialog-win" 
    "dm:lib-gui;analyse"      ; the analyser tool
    "dm:lib-gui;dm-var-dialog"; the dm-var dialog tool
    "dm:lib-gui;exporter"     ; the exporter tool
    "dm:lib-gui;log-manager-win" ; load the new log-manager tool
    "dm:lib-gui;logs"
    "dm:lib-gui;musical-parameters-dialog-win"
    ;"dm:lib-gui;pattern-matching-dm"     ; the pattern matching project algorithm
    ;"dm:lib-gui;pattern-matching-dialog" ; the pattern matching project control panel
    ;"dm:lib-gui;winwin"
    "dm:lib-gui;about-window-win"
    "dm:lib-gui;accentdialog-ebrp-win"
    )))

#+:mcl
(setf *dm-def-files* (append *dm-def-files* 
  '(
    ;;"dm:lib-core;midibasic-mac" 
    "dm:lib-core;midishare-interfacemac.lsp"
    "dm:lib-core;midishare-extensionmac.lsp"
    "dm:lib-core;player-interfacemac.lisp"
    "dm:lib-core;midibasic-midishare.lsp"
 
   ; "dm:lib-core;midifileoutput" 
   ; "dm:lib-core;midifileinput" 
    "dm:lib-gui;midifile-input-dialog-mac" 
    "dm:lib-gui;musicmenus-mac" 
    "dm:lib-gui;utilitydialog-mac"  
    "dm:lib-gui;drawbasic-mac"
    "dm:lib-gui;drawpolynotes-mac"
    ;"dm:lib-gui;draweditparam-mac"
    "dm:lib-gui;drawprop-mac"
    "dm:lib-gui;ruledialog-mac"
    "dm:lib-gui;musicdialog-mac"
  )))

#+(and :mswindows :allegro)
(setf *dm-def-files* (append *dm-def-files* 
  '(
  "dm:lib-gui;applying.bil"   ; going to change end of april !!! /vc
  "dm:lib-gui;analyse.bil"      ; necessary ?
  )))

;#+(and :mswindows :allegro)
;(if (not *demo-version*) (setf *dm-def-files* (append *dm-def-files* ; load some useful tools: text format conversions,...
;  '("dm:pcstuff;mac-to-pc") )))    
    
#+:mcl
(if (not *demo-version*)(setf *dm-def-files* (append *dm-def-files* 
  '("dm:macstuff;pc-mac-conversion") )))

(if *notesenses-version* (setf *dm-def-files* (append *dm-def-files* 
  '("dm:notesenseslib;write-perf-as-sms-3"
    "dm:notesenseslib;ringbatch") )))

;; ==================
;;  LOAD THE SYSTEM
;; ==================


#+:mcl
(dolist (file *dm-def-files*)  ;load the system
  (load-last-version file))

#+:allegro
(dolist (file *dm-def-files*)  ;load the system
  (excl:compile-file-if-needed file)
  (load file) )       

#+:lispworks
(dolist (file *dm-def-files*)  ;load the system
  (hcl:compile-file-if-needed file :load t))


(setq *env-settings* (make-instance 'environment-settings))  ;*********** save/load status

#+:mswindows ;this will start the file dialogs in the same folder as the program
(when *demo-version*
   (set-dm-var 'lib-directory nil)       
   (set-dm-var 'rules-directory nil)       
   (set-dm-var 'rule-sets-directory nil)       
   (set-dm-var 'music-directory nil)       
   (set-dm-var 'log-directory nil)       
   (set-dm-var 'current-music-directory nil)       
   (set-dm-var 'current-rule-sets-directory nil)
   )

#| flyttat
#+(and :mswindows :allegro)
(progn 
  (load "dm:lib-gui;applying.bil")   ; going to change end of april !!! /vc
  (load "dm:lib-gui;analyse.bil")      ; necessary ?
  (if (not *demo-version*) (load "dm:pcstuff;mac-to-pc.lsp"))    ; load some useful tools: text format conversions,...
  (if *notesenses-version* (load "dm:notesenseslib;write-perf-as-sms-3.lsp"))
  (if *notesenses-version* (load "dm:notesenseslib;ringbatch.lsp"))
     )
  
#+:mcl
(if (not *demo-version*) (load "dm:macstuff;pc-mac-conversion"))
|#

;; -----------------------
;;   STARTING THE SYSTEM
;; -----------------------
;;

#+:mcl
(install-music-menus)

#+:mswindows
(if (not *demo-version*) (display-main-window))


#+:mcl
(progn
  ;(add-startup-action #'install-midishare-interface)
  ;(add-quit-action #'remove-midishare-interface)
  (if (not (member 'install-midishare-interface *lisp-startup-functions*))
    (setq *lisp-startup-functions* (append (list 'install-midishare-interface) *lisp-startup-functions* )))
  (if (not (member 'remove-midishare-interface *lisp-cleanup-functions*))
    (setq *lisp-cleanup-functions* (append (list 'remove-midishare-interface) *lisp-cleanup-functions* )))
  (if (not (member 'midi-close *lisp-cleanup-functions*))
    (setq *lisp-cleanup-functions* (append (list 'midi-close) *lisp-cleanup-functions* )))
  (install-midishare-interface)
  (midi-open))

#+:mswindows
(defun dm-exit-functions (x)
   x
   ;(midi-close)
   )

;#+:mswindows
;(if (not (member 'dm-exit-functions *session-exit-fns*))
;    (setq *session-exit-fns* (append (list 'dm-exit-functions) *session-exit-fns* )))

; (setq *package* (find-package "DM"))  ;not working in either system
 

;------------------------------------------------
;  system utilities
;-------------------------------------------------

#+:mcl
(defun select-dm ()
  (setq *package* (find-package "DM")))

;eof

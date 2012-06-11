;;;-*-Mode: LISP; Package: DM -*-
;;
;; ********************************************
;;   Rule dialog and rule setups: common part
;; ********************************************
;;
;;   open-rule-set
;;   default-open-rule-set
;;   rule-call-list-to-string
;;   string-to-rule-call-list
;;   get-top-rule-window

(in-package :dm)

;; -----------------
;;   LOAD-RULES
;; -----------------
;;
;; from the menu
;;
(defun load-rules ()
   (if (or (not (get-dm-var 'current-rule-sets-directory))
           (not (probe-file (get-dm-var 'current-rule-sets-directory))))
      (set-dm-var 'current-rule-sets-directory (get-dm-var 'rule-sets-directory)))
   (let ((file (show-dialog-for-opening-files-PD
                "Load rules"
                :directory (get-dm-var 'current-rule-sets-directory)
                :extensions '(("RUL rule files" . "*.rul")("All files" . "*.*")) )))
      (when file 
         (load file)
         (set-dm-var 'current-rule-sets-directory (directory-namestring file))
         ;(edit-rule-set :pathname file)
         )))

;; -----------------
;;   OPEN-RULE-SET
;; -----------------
;;
;; from the menu
;;
(defun open-rule-set ()
   (if (or (not (get-dm-var 'current-rule-sets-directory))
           (not (probe-file (get-dm-var 'current-rule-sets-directory))))
      (set-dm-var 'current-rule-sets-directory (get-dm-var 'rule-sets-directory)))
   (let ((file (show-dialog-for-opening-files-PD
                "Load a rule palette"
                :directory (get-dm-var 'current-rule-sets-directory)
                :extensions '(("PAL palette files" . "*.pal")("All files" . "*.*")) )))
      (when file 
         (load file)
         (set-dm-var 'current-rule-sets-directory (directory-namestring file))
         (edit-rule-set :pathname file)
         )))


; TEST
;   (set-dm-var 'current-rule-sets-directory (translate-logical-pathname "dm:scores;"))
; (directory-namestring "D:\\u\\andersf\\mac\\dm\\rulepalettes\\jazz.pal")


;; -----------------
;;   OPEN-RULE-SET-FPATH
;; -----------------
;;

(defun open-rule-set-fpath (file)
         (load file)
         (set-dm-var 'current-rule-sets-directory (directory-namestring file))
         (edit-rule-set :pathname file)
         )


;; -------------------------
;;   OPEN-DEFAULT-RULE-SET
;; -------------------------
;;
;; from the menu
;;
(defun open-default-rule-set ()
  (set-dm-var 'all-rules (get-dm-var 'all-rules-default))
  (set-dm-var 'sync-rule-list (get-dm-var 'sync-rule-list-default))
  (edit-rule-set))


; ----------------------------
;   STRING-TO-RULE-CALL-LIST 
; ----------------------------
;
; construct the rule call list
; rule-name-string contains the rule name and optional
; input settings to the rule such as key word inputs
;
(defun string-to-rule-call-list (rule-name-string quant)
  (let (l name index)
    (multiple-value-setq (name index) (read-from-string rule-name-string)) ;get rule name
    (newr l name)
    (newr l quant)
    (when (not (>= index (length rule-name-string)))
      (setq rule-name-string (subseq rule-name-string index))
      (untilexit loop
                 (multiple-value-setq (name index) (read-from-string rule-name-string))
                 (newr l name)
                 (if (>= index (length rule-name-string)) (return))
                 (setq rule-name-string (subseq rule-name-string index))) )
    l))




;; ----------------------------
;;   RULE-CALL-LIST-TO-STRING 
;; ----------------------------
;;
;; ***** Oh My God !!!!!!!!!!!!!
;; using list-to-delimited-string ??? 
;;
(defun rule-call-list-to-string (rule-call-list)
  (let ((s (write-to-string 
             (append (list (car rule-call-list)) (cddr rule-call-list))
             :pretty nil)))
    (subseq s  1  (1- (length s)))) )

;(rule-call-list-to-string '(rulename 1 :ampscale 3))

;; ----------------------------
;;   GET-FOREMOST-RULE-WINDOW 
;; ----------------------------

#+:mswindows
(defun get-top-rule-window ()
  (find-window :apply-rules-window) )
;#+:mcl  
;(defun get-top-rule-window ()
;  (car (windows :class 'display-window)) )
#+:mcl  
(defun get-top-rule-window ()
  (car (windows :class 'apply-rules-window)) )
   





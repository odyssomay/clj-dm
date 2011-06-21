(in-package "DM")

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

(defvar *env-settings* (make-instance 'environment-settings)) ;default environment variables object
(defvar *active-score*)  ;default score

;; =============================
;; Clojure glue
;; =============================

(defun get-active-score ()
  (let ((str (make-string-output-stream)))
    (print-to-file *active-score* str)
    (get-output-stream-string str)))

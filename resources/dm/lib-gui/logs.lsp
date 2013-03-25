;;;-*-Mode: LISP; Package: DM -*-
;;
;; *************************
;;   Log - common routines
;; *************************
;;
;; this file contains routine to create log-file of rule system application
;; these features may be common to Win and Mac.
;; The log-manager to read logs from file is for Win95 only instead now 
;;
;; 9712 /Vittorio Colombo
;;
;; fill-in-logs


(in-package :dm)


;; ---------------
;;  FILL-IN-LOGS
;; ---------------
;;
(defun fill-in-log (*i* prop value &key action target)
   (when (get-dm-var 'log-to-file-enabled)
      (case action
        ('add (case target
                ('current-note (write-to-log "Note @~3A: add to ~A for $~3A, current note : ~A~%" *i* prop *i* value))
                ('next-note (write-to-log "Note @~3A: add to ~A for $~3A, next note  : ~A~%" *i* prop (1+ *i*) value))
                ('previous-note (write-to-log  "Note @~3A: add to ~A for $~3A, previous note : ~A~%" *i* prop (1- *i*) value))
                ('2-notes-ahead (write-to-log  "Note @~3A: add to ~A for $~3A, 2 notes ahead : ~A~%" *i* prop (+ *i* 2) value))
                ('2-notes-back (write-to-log "Note @~3A: add to ~A for $~3A, 2 notes back : ~A~%" *i* prop (- *i* 2) value))
                ('3-notes-ahead (write-to-log "Note @~3A: add to ~A for $~3A, 3 notes ahead : ~A~%" *i* prop (+ *i* 3) value))
                ('3-notes-back (write-to-log "Note @~3A: add to ~A for $~3A, 3 notes back : ~A~%" *i* prop (- *i* 3) value))
                ))
        ('set (case target
                ('current-note (write-to-log "Note @~3A: set ~A for $~3A, current note : ~A~%" *i* prop *i* value))
                ('next-note (write-to-log "Note @~3A: set ~A for $~3A, next note  : ~A~%" *i* prop (1+ *i*) value))
                ('previous-note (write-to-log "Note @~3A: set ~A for $~3A, previous note : ~A~%" *i* prop (1- *i*) value))
                ('2-notes-ahead (write-to-log "Note @~3A: set ~A for $~3A, 2 notes ahead : ~A~%" *i* prop (+ *i* 2) value))
                ('2-notes-back (write-to-log "Note @~3A: set ~A for $~3A, 2 notes back : ~A~%" *i* prop (- *i* 2) value))
                ('3-notes-ahead (write-to-log "Note @~3A: set ~A for $~3A, 3 notes ahead : ~A~%" *i* prop (+ *i* 3) value))
                ('3-notes-back (write-to-log "Note @~3A: set ~A for $~3A, 3 notes back : ~A~%" *i* prop (- *i* 3) value))
                ))
        )
      )
   )


;; --------------
;;  OPEN-LOGFILE for writing
;; --------------
;;
;;
(defmethod open-logfile ((logfile logfile))
   (let ((fpath (show-dialog-for-saving-files-PD "Opening the log file"
                 ;:directory (get-dm-var 'log-directory)
                 :extensions '(("LOG files" . "*.log")("All files" . "*.*")) )))
      (when fpath (setf (fpath logfile) fpath)
         (setf (filehandle logfile)
               (open fpath :direction :output 
                 :if-does-not-exist :create 
                 :if-exists :rename))
         )))


;; ---------------
;;  CLOSE-LOGFILE
;; ---------------
;;
(defmethod close-logfile ((logfile logfile))
   (close (filehandle logfile))
)


;; ---------------
;;  WRITE-TO-LOG
;; ---------------
;;
(defun write-to-log (&rest param)
       (apply 'format (append (list (get-dm-var 'log-filehandle)) param))
     )




;(defun write-to-log (message &rest a)
;   (format  message a)
;)
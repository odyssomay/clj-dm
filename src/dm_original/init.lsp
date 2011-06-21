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

(defun apply-rules (rulelist-string sync-type)
  (rule-apply-list-sync (read-from-string rulelist-string) 'no-sync))
;			(read-from-string sync-type)))
;  (print rulelist-string)
;  (print sync-type))

(defun get-active-score ()
  (let ((str (make-string-output-stream)))
    (print-to-file *active-score* str)
    (get-output-stream-string str)))

(defun read-active-score (str)
  (let ((stream (make-string-input-stream str)))
    (read-active-score-from-stream stream)))

(defun read-active-score-from-stream (stream)
   (let ((inlist)
         (score (make-instance 'score :score-filename ""))
         (track)
         (old-format-p nil))
;      (with-open-file (ifile fpath :direction :input )
       (let ((ifile stream))
        (setq inlist (read ifile nil))

        (cond
         ((symbolp inlist)  ;first symbol
          (case  inlist
            (mono-track (setq track (make-instance 'mono-track)))
            (voice-track (setq track (make-instance 'voice-track)))
            (t (setq old-format-p t)   ;old format
               (setq track (make-instance 'mono-track :trackname (string inlist)))) )
          (loop             ;the rest
            (setq inlist (read ifile nil))
            (cond ((not inlist)       ;end of file
                   (add-one-track score track)
                   (return) )
                  ((keywordp inlist)      ;track var
                   (cond ((slot-exists-p track (read-from-string (string inlist)))
                          (setf (slot-value track (read-from-string (string inlist)))
                            (case inlist
                              (:synth (make-synth (read ifile nil)))
                              (t (read ifile nil))) ))
                         (t (read ifile nil)
                            (warn "not a track variable: ~A" inlist))))
                  ((symbolp inlist)      ;new track
                   (add-one-track score track)
                   (case  inlist
                     (mono-track (setq track (make-instance 'mono-track)))
                     (voice-track (setq track (make-instance 'voice-track)))
                     (t (setq track (make-instance 'mono-track :trackname (string inlist))))  ;old format
                     ))
                  ((listp inlist)              ;if list new tone
                   (add-one-segment track (make-instance 'segment :var-list (list-to-alist inlist))))
                  (t (error "Not expected in file: ~A" inlist)) )))
         (t (error "Not expected in file: ~A" inlist)) )
                 )
      (setq *active-score* score)
     (if old-format-p (get-track-par))
     (recreate-time-shapes)
      (if (get-dm-var 'verbose-i/o) (print-ll  "Active score loaded"))
      score))

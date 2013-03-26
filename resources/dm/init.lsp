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

(defun filter-segment (segment)
  (remove-if (lambda (p)
			   (cond
				 ((eq (car p) 'DR) nil)
				 ((eq (car p) 'NDR) nil)
				 ((eq (car p) 'N) nil)
				 ((eq (car p) 'REST) nil)
				 ((eq (car p) 'METER) nil)
				 ((eq (car p) 'SL) nil)
				 ((eq (car p) 'DOT) nil)
				 ((eq (car p) 'MM) nil)
				 ((eq (car p) 'KEY) nil)
				 ((eq (car p) 'BAR) nil)
				 (t t))) segment))

(defun get-filtered-track (index)
  (map 'list #'var-list (segment-list (nth index (track-list *active-score*)))))

(defun read-score-from-string (string)
   (let ((inlist)
         (score (make-instance 'score :score-filename "no name"))
         (track)
         (old-format-p nil))
      (with-input-from-string (ifile string)
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
;      (if (get-dm-var 'verbose-i/o) (print-ll  "Active score loaded from " fpath))
      score))

(defun get-midi-from-score ()
  (let ((str (make-string-output-stream)))
    (save-performance-midifile1-stream str)
    (get-output-stream-string str)))

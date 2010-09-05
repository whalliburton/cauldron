;; system.lisp

(in-package :hardware)

(defun truncate-string (string length &optional (suffix "..."))
  (if (> (length string) length)
    (concatenate 'string (subseq string 0 length) suffix)))

(defun system ()
  "List various sundry system properties."
  (print-table 
   (mapcar (lambda (el) (list (first el) (typecase (second el)
                                           (string (truncate-string (second el) 70))
                                           (cons (second el)))))
           (list-system-information))))

(defun list-system-information ()
  `((:version ,(first (slurp-lines "/proc/version")))
    (:uptime ,(split-sequence #\space (first (slurp-lines "/proc/uptime"))))
    (:loadavg ,(split-sequence #\space (first (slurp-lines "/proc/loadavg"))))))




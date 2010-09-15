;; frequency.lisp

(in-package :language)

(defparameter *frequency-filename* "/lisp/projects/language/russian/5000-frequency.txt")

(defun load-word-frequency-list ()
  (iter (for line in-file *frequency-filename* using #'read-line)
        (collect (cddr (split-sequence #\space (string-trim '(#\return) line))))))


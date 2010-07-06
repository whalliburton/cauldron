;; pretty-printing.lisp

(in-package :utilities)

(defun print-heading (string &optional (stream *standard-output*))
  (princ string stream)
  (newline)
  (dotimes (x (length string))
    (write-char #\= stream))
  (newline 2))

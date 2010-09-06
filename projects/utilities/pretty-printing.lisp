;; pretty-printing.lisp

(in-package :utilities)

(defun print-heading (string &optional (stream *standard-output*) (newlines 2))
  (princ string stream)
  (newline)
  (dotimes (x (length string))
    (write-char #\= stream))
  (newline newlines))

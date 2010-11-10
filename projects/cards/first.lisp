;; first.lisp

(in-package :cards)

(defun stamp ()
  (multiple-value-bind (nsec ss mm hh day month year dow) (decode-timestamp (now))
    (format nil "~a-~a-~a-~2,'0d" hh day month (mod year 100))))

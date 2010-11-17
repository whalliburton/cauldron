;; run.lisp

(in-package :utilities)

(defun run-to-string (filename &rest args)
  (with-output-to-string (str)
    (run-program filename args :output str)))
;; run.lisp

(in-package :utilities)

(defun run (filename &rest args)
  (with-output-to-string (str)
    (run-program filename args :output str)))
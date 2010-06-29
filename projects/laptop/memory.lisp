;; memory.lisp

(in-package :laptop)

(defun memory-information ()
  (print-table 
   (mapcar (lambda (el)
             (mapcar (lambda (el) (string-trim '(#\space) el)) (split-sequence #\: el)))
           (slurp-lines "/proc/meminfo"))
   :right-justified t))



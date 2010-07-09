;; memory.lisp

(in-package :laptop)

(defun memory-information ()
  (print-table (process-string (slurp-lines "/proc/meminfo") '((:split #\:) (:trim #\space)))
               :right-justified t))




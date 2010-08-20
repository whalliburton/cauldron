;; memory.lisp

(in-package :hardware)

(defun memory-information ()
  (print-table (process-string (slurp-lines "/proc/meminfo") '((:split #\:) (:trim #\space)))
               :right-justified t))




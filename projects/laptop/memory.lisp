;; memory.lisp

(in-package :laptop)

(defun memory-information ()
  (print-table (slurp-and-split-on-colon "/proc/meminfo") :right-justified t))



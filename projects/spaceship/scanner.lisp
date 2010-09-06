;; scanner.lisp

(in-package :spaceship)

(defun short-range-scan ()
  "Scan the immediate area."
  (battery)
  (newline)
  (cpu-info))

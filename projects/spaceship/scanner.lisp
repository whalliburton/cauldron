;; scanner.lisp

(in-package :spaceship)

(defun short-range-scan ()
  "Scan the immediate area."
  (newline)
  (print-table (nconc (battery :as-list t)
                      (blank-table-line)
                      (destructuring-bind-list (cpu freq min max) (cpus :as-list t)
                        (list (format nil "CPU~A" cpu)
                              (format nil "~D MHz" (/ freq 1000))
                              (format nil "(~D MHz max)" (/ max 1000))))))
  (newline))

(defalias srs short-range-scan)
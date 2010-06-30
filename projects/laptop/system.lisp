;; system.lisp

(in-package :laptop)

(defun system-information ()
  `((:version ,(first (slurp-lines "/proc/version")))
    (:uptime ,(split-sequence #\space (first (slurp-lines "/proc/uptime"))))
    (:loadavg ,(split-sequence #\space (first (slurp-lines "/proc/loadavg"))))))




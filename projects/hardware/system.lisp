;; system.lisp

(in-package :hardware)

(defun system-information ()
  "List various sundry system properties."
  `((:version ,(first (slurp-lines "/proc/version")))
    (:uptime ,(split-sequence #\space (first (slurp-lines "/proc/uptime"))))
    (:loadavg ,(split-sequence #\space (first (slurp-lines "/proc/loadavg"))))))




;; packages.lisp

(defpackage timer
  (:use common-lisp cl-cairo2 cl-colors graphics local-time)
  (:import-from sb-ext quit list-all-timers make-timer schedule-timer unschedule-timer))

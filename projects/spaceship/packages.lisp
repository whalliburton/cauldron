;; packages.lisp

(defpackage spaceship
    (:use common-lisp sb-thread iterate alexandria
          utilities databases cards hardware web music
          network language linux journal documents
          communications windows bknr.datastore
          paint-client)
  (:import-from sb-ext quit run-program posix-getenv make-timer
                schedule-timer unschedule-timer timer-name
                timer-scheduled-p
                list-all-timers)
  (:export help short-range-scan srs universal-time
           countdown acknowledge-countdowns list-countdowns cancel-countdown))

(in-package :spaceship)

(defparameter *help-text* "The bridge.")











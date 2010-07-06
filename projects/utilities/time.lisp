;; time.lisp

(in-package :utilities)

(defun seconds-to-days-hours-minutes-seconds (seconds)
  (multiple-value-bind (hours hour-remainder) 
      (floor seconds +seconds-per-hour+)
    (multiple-value-bind (days hours) 
        (floor hours 24)
      (multiple-value-bind (minutes seconds)
          (floor hour-remainder +seconds-per-minute+)
        (values days hours minutes seconds)))))

(defun seconds-to-duration-string (total-seconds)
  (multiple-value-bind (days hours minutes seconds) 
      (seconds-to-days-hours-minutes-seconds total-seconds)
    (format nil "~4D:~2,'0D:~2,'0D:~2,'0D" days hours minutes (round seconds))))

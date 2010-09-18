;; monitors.lisp

(in-package :spaceship)

(defun battery-monitor ()
  (iter (for battery in (list-batteries))
        (when (< (battery-percentage battery) 20)
          (paint `(alert ,(format nil "Battery at ~,2f%" (battery-percentage battery))))
          (play-notes '(c e g)))))

(defun start-monitors ()
  (schedule-timer (make-timer 'battery-monitor :name "battery monitor") 
                  0 :repeat-interval (* 60 10)))


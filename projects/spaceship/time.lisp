;; time.lisp

(in-package :spaceship)

(defun universal-time ()
  "The number of seconds since seconds since midnight, 1/1/1900 GMT."
  (format t "~A" (get-universal-time)))

(defvar *countdown-timers* nil)
(defvar *countdown-timers-lock* (make-mutex :name "countdown timers"))

(defun timer-acknowledgment-handler ()
  (play-notes '(a a a b c)))

(defvar *acknowledgment-timer* (make-timer #'timer-acknowledgment-handler :thread t))

(defun possibly-schedule-reminder ()
  (unless (timer-scheduled-p *acknowledgment-timer*)
    (schedule-timer *acknowledgment-timer* 10 :repeat-interval 10)))

(defun delete-countdown-timer (timer)
  (with-recursive-lock (*countdown-timers-lock*)
    (setf *countdown-timers* (delete timer *countdown-timers*))))

(defun list-countdowns ()
  "List all the active and unacknowledged countdown alarms."
  (let ((now (get-internal-real-time)))
    (print-table
     (iter (for timer in *countdown-timers*)
           (for index from 1)
           (collect (list index
                          (timer-name timer )
                          (if (timer-scheduled-p timer)
                            (round (- (sb-impl::%timer-expire-time timer) now)
                                   internal-time-units-per-second)
                            "finished"))))
     :headings '("index" "name" "seconds"))))

(defun cancel-countdown (index)
  "Cancel a running countdown alarm."
  (if-let (timer (nth (1- index) *countdown-timers*))
    (progn
      (unschedule-timer timer)
      (delete-countdown-timer timer)
      (format t "Countdown ~S has been canceled.~%" (timer-name timer)))
    (error "No countdown timer with index ~A" index)))

(defun default-countdown-finished-handler ()
  (play-notes '(a b b c))
  (possibly-schedule-reminder))

(defun time-to-seconds (length unit)
  (* length
     (ecase unit
       (:seconds 1)
       (:minutes 60)
       (:hours (* 60 60))
       (:days (* 60 60 24)))))

(defun countdown (&optional length
                            (name "countdown timer")
                            (unit :minutes)
                            (handler #'default-countdown-finished-handler))
  "Schedule a countdown alarm or list existing countdowns."
  (if (null length)
    (list-countdowns)
    (let ((timer (make-timer handler :name name)))
      (schedule-timer timer (time-to-seconds length unit))
      (with-mutex (*countdown-timers-lock*)
        (push timer *countdown-timers*))
      (format t "Countdown ~S scheduled for ~A ~(~A~).~%" name length unit))))

(defun acknowledge-countdowns ()
  "Acknowledge all the finished countdown alarms."
  (with-mutex (*countdown-timers-lock*)
    (iter (for timer in *countdown-timers*)
          (unless (timer-scheduled-p timer)
            (format t "countdown ~S acknowledged~%" (timer-name timer))
            (delete-countdown-timer timer)))
    (unschedule-timer *acknowledgment-timer*)))

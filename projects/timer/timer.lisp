;; timer.lisp

(in-package :timer)

(defparameter *timer-window* nil)

(defun create-timer ()
  (setf *timer-window* (make-instance 'updating-window :name "timer" :update-function 'draw-timer)))

(defun draw-timer (window)
  (let* ((*context* (context window)))
    (fill-with-color window +white+)
    (select-font-face "Sans" :normal :bold)
    (set-font-size 30)
    (move-to 30 150)
    (set-color +magenta4+)
    (show-text (format-timestring nil (now) :format +asctime-format+))
    (stroke)))

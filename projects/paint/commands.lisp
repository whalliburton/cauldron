;; commands.lisp

(in-package :paint)

(defmacro with-window ((var &rest args) &body body)
  `(let* ((,var (make-instance 'window ,@args))
          (*context* (context ,var)))
     ,@body))

(defun alert (text)
  (assert (typep text 'string))
  (with-window (window :name "alert" :width 400 :height 100)
    (fill-with-color window +red+)
    (select-font-face "Sans" :normal :bold)
    (set-font-size 30)
    (set-color +white+)
    (move-to 20 50)
    (show-text text)
    (schedule-timer (make-timer (lambda () (close-window window))) 5)))


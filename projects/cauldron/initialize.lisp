;; initialize.lisp

(in-package :cauldron)

(defun initialize ()
  (start-udev-monitor)
  (format t "Welcome to cauldron."))



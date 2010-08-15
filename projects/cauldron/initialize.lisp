;; initialize.lisp

(in-package :cauldron)

(defun initialize ()
  (setup-data-directory)
  (start-udev-monitor)
  (format t "~%Welcome to cauldron.~%~%"))



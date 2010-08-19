;; initialize.lisp

(in-package :spaceship)

(defun initialize ()
  (setup-data-directory)
  (start-udev-monitor)
  (format t "~%Welcome to the spaceship.~%~%"))



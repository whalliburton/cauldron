;; initialize.lisp

(in-package :spaceship)

(defun initialize ()
  (start-database)
  (start-udev-monitor)
  (format t "~%Welcome to the spaceship.~%~%"))



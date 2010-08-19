;; initialize.lisp

(in-package :spaceship)

(defun initialize ()
  (start-database)
  (start-udev-monitor)
  (initialize-music)
  (format t "~%Welcome to the spaceship.~%~%"))



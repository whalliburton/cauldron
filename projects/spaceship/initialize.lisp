;; initialize.lisp

(in-package :spaceship)

(defun initialize ()
  (start-local-database ".spaceship")
  (start-udev-monitor)
  (initialize-music)
  (format t "~%Welcome to the spaceship.~%~%"))



;; initialize.lisp

(in-package :spaceship)

(defun initialize ()
  (start-local-database ".spaceship")
  (start-udev-monitor)
  (initialize-music)
  (start-shell-control-monitor)
  (load-email-identity)
  (start-paint-server)
  (start-monitors)
  (format t "~%Welcome to the spaceship.~%~%"))



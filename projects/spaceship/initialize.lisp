;; initialize.lisp

(in-package :spaceship)

(defun initialize ()
  (setup-data-directory)
  (start-local-database (database-directory))
  (start-udev-monitor)
  (initialize-music)
  (start-shell-control-monitor)
  (load-email-identity)
  (start-paint-server)
  (start-monitors)
  (format t "~%Welcome to the spaceship.~%~%"))



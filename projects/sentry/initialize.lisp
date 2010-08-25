;; initialize.lisp

(in-package :sentry)

(defun initialize ()
  (format t "Starting the sentry.~%")
  (start-swank-server)
  (start-local-database ".sentry")
  (connect-to-irc)
  (start-server)
  (iter (sleep 10)))

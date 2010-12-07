;; time.lisp

(in-package :spaceship)

(defun universal-time ()
  "The number of seconds since seconds since midnight, January 1, 1900 GMT."
  (format t "~A" (get-universal-time)))

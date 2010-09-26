;; repl.lisp

(in-package :utilities)

(defvar *debugging-globals* nil)

(defun clear-debugging-globals ()
  (mapc #'makunbound *debugging-globals*)
  (setf *debugging-globals* nil)
  (format t "cleared all debugging globals.~%"))

(defmacro sg (&optional name)
  "Set a global named NAME to the last REPL result."
  (cond 
    ((null name) (clear-debugging-globals))
    (t (let ((already (member name *debugging-globals*)))
         (if (and (boundp name) (not already))
           (warn "~a is not available." name)
           (progn
             (when already (warn "Resetting ~a." name))
             (pushnew name *debugging-globals*)
             `(defparameter ,name *)))))))


;; server.lisp

(in-package :paint)

(defvar *paint-server*)

(defun start-server ()
  (setf *paint-server* 
        (make-instance 'simple-server 
                       :handler 'paint-server-handler
                       :port 5021)))

(defparameter *commands* '(alert))

(defun paint-server-handler (command)
  (handler-case 
      (progn
        (unless (consp command)
          (error "Invalid command: ~S." command))
        (unless (member (car command) *commands*)
          (error "Unknown command: ~S." command))
        (apply (car command) (cdr command)))
    (error (e)
      (list 'error (princ-to-string e)))))


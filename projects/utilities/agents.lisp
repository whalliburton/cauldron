;; agents.lisp

(in-package :utilities)

(defparameter *agents* nil)

(defclass agent ()
  ((name :initarg :name :reader name)
   (process :initarg :process :reader process)
   (alive :initform t)))

(defun alivep (agent)
  (when (slot-value agent 'alive)
    (if (process-poll (process agent))
      (setf (slot-value agent 'alive) nil)
      t)))

(defmethod print-object ((agent agent) stream)
  (print-unreadable-object (agent stream :type t)
    (format stream "~A ~A ~A" (name agent) (process-pid (process agent))
            (alivep agent))))

(defun start-agent (name command-line)
  (when (find name *agents* :key #'name)
    (error "An agent named ~A already exists." name))
  (push (make-instance 'agent 
                       :name name
                       :process (create-process command-line :union-stdout-stderr t))
        *agents*))

(defun list-agents ()
  (print-table (mapcar (lambda (el) (list (name el)
                                          (process-pid (process el)) 
                                          (alivep el))) *agents*)))

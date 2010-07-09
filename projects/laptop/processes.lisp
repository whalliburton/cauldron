;; processes.lisp

(in-package :laptop)

(defclass process ()
  ((pid :initarg :pid)
   (name :initarg :name)
   (status :initarg :status)))

(defmethod print-object ((process process) stream)
  (with-slots (pid name) process
    (print-unreadable-object (process stream :type t)
      (format stream "~A ~A" pid name))))

(defun proc-status (pid)
  (process-string (slurp-lines (format nil "/proc/~a/status" pid))
                  '((:split #\:) (:trim #\space #\tab))))

(defmethod initialize-instance :after ((process process) &rest rest)
  (declare (ignore rest))
  (with-slots (pid status name) process
    (setf status (proc-status pid)
          name (cadr (assoc "Name" status :test #'string=)))))

(defun processes ()
  (mapcan 
   (lambda (el) 
     (awhen (ignore-errors (parse-integer (last1 (pathname-directory el))))
       (list (make-instance 'process :pid it))))
   (list-directory "/proc/")))

(defun describe-process (pid)
  (print-table (slot-value (make-instance 'process :pid pid) 'status)))
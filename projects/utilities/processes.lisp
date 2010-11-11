;; processes.lisp

(in-package :utilities)

(defun process-lines (command-line)
 (with-child-process (process command-line :stdout t)
    (iter (for line = (read-line (process-output process) nil nil))
          (while line)
          (collect line))))

(defmacro iterate-process-lines ((var command-line) &body body)
  `(with-child-process (process ,command-line :stdout t)
     (iter (for ,var = (read-line (process-output process) nil nil))
           (while ,var)
           ,@body)))

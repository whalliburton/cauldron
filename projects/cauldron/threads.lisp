;; threads.lisp

(in-package :cauldron)

(defun print-threads ()
  (print-table 
   (iter 
     (for thread in (list-all-threads))
     (collect (list (thread-name thread)
                    (if (thread-alive-p thread) "alive" "dead"))))))

;; hash-tables.lisp

(in-package :utilities)

(defparameter *maximum-hash-table-pretty-print-size* 25)

(defmethod print-object ((hash-table hash-table) stream)
  (let ((*print-pretty* nil))
    (iter (initially (write-string "#{" stream))
          (for (key value) in-hashtable hash-table)
          (for x upfrom 0)
          (unless (first-iteration-p) (write-char #\space stream))
          (when (and *maximum-hash-table-pretty-print-size* 
                     (>= x *maximum-hash-table-pretty-print-size*))
            (write-string "..." stream) 
            (finish))
          (princ key stream)
          (write-char #\space stream)
          (princ value stream)
          (finally (write-char #\} stream)))))

(defmacro with-hash-values ((keys hash &key (to-key nil to-key-set)) &body body)
  (once-only (hash to-key) 
    `(let (,@(mapcar 
              (lambda (key) 
                (list key `(gethash 
                            ,@(if to-key-set
                                `((funcall ,to-key ',key))
                                `(',key))
                            ,hash)))
               keys))
       ,@body)))

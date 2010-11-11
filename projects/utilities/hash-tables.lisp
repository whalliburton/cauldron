;; hash-tables.lisp

(in-package :utilities)

(defun print-hash-table (hash-table)
  (print-table (iter (for (k v) in-hashtable hash-table) (collect (list k v)))))

(defun hashtable-reader (s c n)
  (declare (ignore c n))
  (iter (with table = (make-hash-table))
        (with list = (read-delimited-list #\} s t))
        (for (key value) on list by #'cddr)
        (setf (gethash key table) value)
        (finally (return table))))

(defparameter *maximum-hash-table-pretty-print-size* 25)

(defparameter *hash-table-prefix* "")

(defmethod print-object ((hash-table hash-table) stream)
  (let ((*print-pretty* nil))
    (multiple-value-bind (vals long)
        (iter (for (key value) in-hashtable hash-table)
              (for x upfrom 0)
              (when (and *maximum-hash-table-pretty-print-size*
                         (>= x *maximum-hash-table-pretty-print-size*))
                (return (values rtn t)))
              (collect key into rtn)
              (collect value into rtn)
              (finally (return rtn)))
      (format stream "#~A{~{~s~^ ~}" *hash-table-prefix* vals)
      (when long (format stream " ..."))
      (write-char #\} stream))))

(set-dispatch-macro-character #\# #\{ #'hashtable-reader)

(set-macro-character #\} (get-macro-character #\) nil))

;; expiring-hash-table - a hash table whos elements time out and are
;; removed. Resetting a key does not reset the timer. Checks for
;; expirations on setting; if this is too heavy we could check ever
;; 100 or so sets.

(defclass expiring-hash-table ()
  (table fifo lock
   (timeout :initform 10 :initarg :timeout)))

(defmethod print-object ((table expiring-hash-table) stream)
  (let ((*hash-table-prefix* "E"))
    (print-object (slot-value table 'table) stream)))

(defmethod initialize-instance :after
    ((expiring-hash-table expiring-hash-table) &key (test 'eql))
  (with-slots (table fifo thread lock timeout) expiring-hash-table
    (setf table (make-hash-table :test test)
          fifo (make-fifo)
          lock (make-mutex :name "expiring-hash-table"))))

(defun set-expiring-hash (hash key value)
  (with-slots (table lock fifo timeout) hash
    (with-mutex (lock)
      (setf (gethash key table) value)
      (fifo-enqueue (cons (get-universal-time) key) fifo)
      (iter (with now = (get-universal-time))
            (for head = (fifo-head fifo))
            (while (and head (> (- now (caar head)) timeout)))
            (remhash (cdar head) table)
            (fifo-dequeue fifo))))
  value)

(defun gethash-expiring (key hash)
  (with-slots (table lock) hash
    (with-mutex (lock)
      (gethash key table))))

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

(defun list-hash-keys (hash-table)
  (iter (for (k v) in-hashtable hash-table) (collect k)))

(defun maphash-expiring (fn expiring-hash-table)
  (with-slots (table) expiring-hash-table
    (iter (for (k v) in-hashtable table) (funcall fn k v))))

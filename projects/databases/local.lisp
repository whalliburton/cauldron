;; local.lisp

(in-package :databases)

(defun ensure-local-database-directory (name)
  (ensure-directories-exist 
   (concatenate 'string
                (or (posix-getenv "HOME")
                    (error "HOME is not set."))
                "/" name "/")))

(defun start-local-database (name)
  (assert name)
  (make-instance 'mp-store :directory (ensure-local-database-directory name)
                 :subsystems (list (make-instance 'store-object-subsystem)
                                   (make-instance 'blob-subsystem))))

(defclass database-key-value (store-object)
  ((key :initarg :key
        :index-type unique-index
        :index-initargs (:test #'equalp)
        :index-reader database-key-value-with-key)
   (value :initarg :value))
  (:metaclass persistent-class))

(defmethod print-object ((obj store-object) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (key value) obj
      (format stream "~S ~S" key value))))

(deftransaction set-database-key-value (obj new-value)
  (setf (slot-value obj 'value) new-value))

(defun (setf gethash-database) (value key)
  (if-let (obj (database-key-value-with-key key))
    (set-database-key-value obj value)
    (make-object 'database-key-value :key key :value value))
  value)

(defun gethash-database (key)
  (if-let (obj (database-key-value-with-key key))
    (values (slot-value obj 'value) t)
    (values nil nil)))

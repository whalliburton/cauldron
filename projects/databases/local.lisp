;; local.lisp

(in-package :databases)

(defun ensure-local-database-directory (name)
  (ensure-directories-exist
   (concatenate 'string
                (or (posix-getenv "HOME")
                    (error "HOME is not set."))
                "/" name "/")))

(defparameter *temporary-blob-directory* nil)

(defun start-local-database (name)
  (assert name)
  (let ((base (ensure-local-database-directory name)))
    (make-instance 'mp-store :directory base
                   :subsystems (list (make-instance 'store-object-subsystem)
                                     (make-instance 'blob-subsystem)))
    (setf *temporary-blob-directory* 
          (ensure-directories-exist (concatenate 'string base "tmp/")))))

(defun create-blob-link (name blob)
  (let ((link-name (concatenate 'string *temporary-blob-directory* name)))  
    (when (probe-file link-name)
      (sb-posix:unlink link-name))
    (sb-posix:link (blob-pathname blob) link-name)
    link-name))

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

(defun packetize-object (obj)
  (let ((class (class-of obj)))
    (iter (with ignore-packages = (list (find-package :bknr.indices)
                                        (find-package :bknr.datastore)))
          (for slot in (closer-mop:class-slots class))
          (for slot-name = (closer-mop:slot-definition-name slot))
          (unless (member (symbol-package slot-name) ignore-packages)
            (when (slot-boundp obj slot-name)
              (collect (cons slot-name (slot-value obj slot-name))))))))

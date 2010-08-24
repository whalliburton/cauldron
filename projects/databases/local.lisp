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

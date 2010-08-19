;; database.lisp

(in-package :spaceship)

(defparameter *spaceship-data-directory-name* ".spaceship")

(defun ensure-data-directory ()
  (ensure-directories-exist 
   (concatenate 'string
                (or (posix-getenv "HOME")
                    (error "HOME is not set."))
                "/" *spaceship-data-directory-name* "/")))

(defun start-database ()
  (make-instance 'mp-store :directory (ensure-data-directory)
                 :subsystems (list (make-instance 'store-object-subsystem))))




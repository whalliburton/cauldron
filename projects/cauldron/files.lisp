;; files.lisp

(in-package :cauldron)

(defparameter *cauldron-data-directory-name* ".cauldron")
(defparameter *cauldron-data-directory* nil)

(defun setup-data-directory ()
  (ensure-directories-exist 
   (setf *cauldron-data-directory* 
         (concatenate 'string
                      (or (posix-getenv "HOME")
                          (error "HOME is not set."))
                      "/" *cauldron-data-directory-name* "/"))))

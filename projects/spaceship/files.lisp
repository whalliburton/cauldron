;; files.lisp

(in-package :spaceship)

(defparameter *spaceship-data-directory-name* ".spaceship")
(defparameter *spaceship-data-directory* nil)

(defun setup-data-directory ()
  (ensure-directories-exist 
   (setf *spaceship-data-directory* 
         (concatenate 'string
                      (or (posix-getenv "HOME")
                          (error "HOME is not set."))
                      "/" *spaceship-data-directory-name* "/"))))

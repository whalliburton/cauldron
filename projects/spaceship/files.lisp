;; files.lisp

(in-package :spaceship)

(defparameter *spaceship-data-directory-name* ".spaceship")
(defparameter *spaceship-data-directory* nil)

(defun display-number ()
  (subseq (or (posix-getenv "DISPLAY")
              (error "No DISPLAY environment variable is set."))
          1))

(defun setup-data-directory ()
  (ensure-directories-exist 
   (setf *spaceship-data-directory* 
         (concatenate 'string
                      (or (posix-getenv "HOME")
                          (error "HOME is not set."))
                      "/" *spaceship-data-directory-name* "/"
                      (display-number) "/"))))

(defun database-directory ()
  (concatenate 'string ".spaceship/" (display-number)))


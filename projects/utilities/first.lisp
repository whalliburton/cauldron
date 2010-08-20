;; first.lisp

(in-package :utilities)

(defun newline (&rest args)
  (if (integerp (car args))
    (iter (repeat (car args)) (apply #'terpri (cdr args)))
    (apply #'terpri args)))

(defun last1 (list)
  (car (last list)))

(defun in-home (suffix)
  (concatenate 'string 
               (or (sb-posix:getenv "HOME")
                   (let ((dir (concatenate 'string  "/tmp/" (random-string))))
                     (progn (warn "No HOME environment set, using ~a" dir) dir)))
               suffix))

(defun safe-read-from-string (string)
  (let ((*read-eval* nil))
    (read-from-string string)))

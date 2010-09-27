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

(defun remove-keyword (list keyword)
  (iter (with remove) 
        (for i in list)
        (cond
          ((eql i keyword) (setf remove t))
          (remove (setf remove nil))
          (t (collect i)))))

(defun remove-keywords (list &rest keywords)
  (iter (for argument in keywords)
        (setf list (remove-keyword list argument))
        (finally (return list))))

(defun parse-float (s)
  (let ((*read-eval* nil))
    (let ((val (read-from-string s)))
      (cond
	((typep val 'float) val)
	((typep val 'fixnum) (coerce val 'float))))))

(defun view-in-web-browser (filename)
  "Open a chromium tab viewing FILENAME."
  (run-program "/usr/bin/chromium" (list filename)))

(defun ensure-string (string-or-symbol)
  (etypecase string-or-symbol
    (string string-or-symbol)
    (symbol (symbol-name string-or-symbol))))

(defun plural (list)
  (not (null (cdr list))))

(defun home (&optional path)
  (pathname (concatenate 'string (namestring (user-homedir-pathname)) path)))


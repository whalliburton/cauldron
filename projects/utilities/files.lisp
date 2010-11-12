;; files.lisp

(in-package :utilities)

(sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
    sb-alien:c-string
  (dir sb-alien:c-string)
  (prefix sb-alien:c-string))

(defun temporary-file-name ()
  "Return a temporary file name."
  (tempnam nil nil))

(defmacro with-temporary-file ((stream filename) &body body)
  `(let ((,filename (temporary-file-name)))
     (with-open-file (,stream ,filename :direction :output)
       (unwind-protect
            (progn ,@body)
         (sb-posix:unlink ,filename)))))


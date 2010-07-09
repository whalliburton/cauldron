;; slurping.lisp

(in-package :utilities)

;; unused
(defun slurp-stream (stream &optional (element-type '(unsigned-byte 8)))
  "Slurp all octets from STREAM. Returns a vector of ELEMENT-TYPE"
  (let ((seq (make-array (file-length stream) :element-type element-type)))
    (read-sequence seq stream)
    seq))

;; unused
(defun slurp (filename &optional (element-type '(unsigned-byte 8)))
  "Slurp the contents of the file designated by FILENAME, returning
a vector of ELEMENT-TYPE."
  (with-open-file (f filename :direction :input
                     :if-does-not-exist :error
                     :element-type element-type)
    (slurp-stream f element-type)))

(defun slurp-lines (filename)
  (with-open-file (f filename :direction :input :if-does-not-exist :error)
    (iter (for line = (read-line f nil))
          (while line)
          (collect line))))

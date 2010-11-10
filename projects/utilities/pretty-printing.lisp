;; pretty-printing.lisp

(in-package :utilities)

(defun print-heading (string &optional (stream *standard-output*) (newlines 2))
  (princ string stream)
  (newline)
  (dotimes (x (length string))
    (write-char #\= stream))
  (newline newlines))

(defun prin1-with-ellipses (object &optional (stream *standard-output*) (max 20)
                                   (printer #'prin1-to-string))
  (typecase object
    (cons
       (write-char #\( stream)
       (iter (with length = 0)
             (for els on object)
             (for el = (car els))
             (for string = (funcall printer el))
             (incf length (1+ (length string)))
             (princ string stream)
             (when (> length max) (princ " ..." stream) (return))
             (when (cdr els) (write-char #\space stream)))
       (write-char #\) stream))
    (t (write-string
        (let ((str (funcall printer object)))
          (if (> (length str) max)
            (concatenate 'string (subseq str 0 (- max 3)) "...")
            str))
        stream)))
  object)
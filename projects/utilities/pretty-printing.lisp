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

(defun prin1-with-ellipses-to-string (obj &optional (max 20) (printer #'prin1-to-string))
  (with-output-to-string (str)
    (prin1-with-ellipses obj str max printer)))

(defun indent (depth &optional (stream *standard-output*))
  (dotimes (x depth) (format stream "  ")))

(defun print-box (str &optional (stream *standard-output*) (indent 0) (newlines t))
  (when newlines (newline))
  (let* ((len (length str))
         (padded (+ len 5)))
    (flet ((line ()
             (indent indent)
             (iter (for x from 0 to padded)
                   (for last-iteration-p = (eql x padded))
                   (write-char
                    (cond
                      ((or (first-iteration-p) last-iteration-p) #\+)
                      (t #\-)) stream)
                   (finally (newline stream)))))
      (line)
      (indent indent)
      (write-string "|  " stream)
      (write-string str stream)
      (write-string "  |" stream)
      (newline stream)
      (line)))
  (when newlines (newline)))

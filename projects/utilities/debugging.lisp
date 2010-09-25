;; debugging.lisp

(in-package :utilities)

(defmacro bugout (&rest vars)
  "Print VARS, for debugging. Keywords are printed as labels."
  `(format t ,(with-output-to-string (s)
                (write-string "~%>>>" s)
                (iter (for var in vars)
                      (write-string "  " s)
                      (prin1 var s)
                      (unless (keywordp var) (write-string " ~S" s)))
                (write-string "~%~%" s))
           ,@(remove-if #'keywordp vars)))

(defun prin1-with-ellipses (list &optional (stream *standard-output*) (max 20))
  (write-char #\( stream)
  (iter (with length = 0)
        (for els on list)
        (for el = (car els))
        (for string = (prin1-to-string el))
        (incf length (1+ (length string)))
        (princ string stream)
        (when (> length max) (princ " ..." stream) (return))
        (when (cdr els) (write-char #\space stream)))
  (write-char #\) stream)
  list)

(defmacro breakout (&rest vars)
  "Break with VARS, for debugging."
  `(break ,(with-output-to-string (s)
             (iter (for var in vars)
                   (unless (first-iteration-p) (write-string "  " s))
                   (typecase s
                     (cons (prin1-with-ellipses var s))
                     (t (prin1 var s)))
                   (unless (keywordp var) (write-string " ~S" s))))
          ,@(remove-if #'keywordp vars)))

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

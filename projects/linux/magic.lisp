;; magic.lisp

(in-package :linux)

(defun magic-mime (filename)
  (string-right-trim '(#\newline)
   (with-output-to-string (stream)
     (when
         (eql (process-exit-code 
               (run-program "/usr/bin/file" (list "-b" "--mime-type" filename) :output stream))
              1)
       (return-from magic-mime)))))
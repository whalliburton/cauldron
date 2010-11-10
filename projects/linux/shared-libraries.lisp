;; shared-libraries.lisp

(in-package :linux)

(define-condition failure-to-list-shared-libraries (error)
  ((output-text :initarg :output-text)
   (exit-code :initarg :exit-code)))

(defun list-shared-libraries (executable-filename)
  "List all the shared libraries used by EXECUTABLE-FILENAME."
  (process-string
   (with-output-to-string (stream)
     (let ((exit-code
            (process-exit-code
             (run-program "/usr/bin/ldd"
                          (list executable-filename) :output stream))))
       (unless (eql exit-code 0)
         (error 'failure-to-list-shared-libraries
                :output-text (get-output-stream-string stream)
                :exit-code exit-code))))
   '((:split #\newline) (:trim #\tab) (:up-to #\space))))

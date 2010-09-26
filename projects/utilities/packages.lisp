;; packages.lisp

(defpackage utilities
  (:use common-lisp iterate split-sequence local-time usocket sb-thread)
  (:import-from sb-ext quit run-program)
  (:import-from alexandria once-only with-gensyms)
  (:import-from iolib.process with-child-process process-output create-process
                process-poll process-pid)
  (:import-from sb-gray fundamental-character-output-stream
                stream-write-char stream-force-output)
  (:import-from fare-utils make-fifo fifo-head fifo-enqueue fifo-dequeue)  
  (:export newline slurp slurp-stream slurp-lines slurp-line last1 run
           string-starts-with string-ends-with string-contains-p
           chew-string process-string print-table print-heading blank-table-line
           pad-string
           seconds-to-duration-string with-assoc
           mkstr symb ksymb in-home
           random-string random-byte-vector
           safe-read-from-string remove-keywords parse-float
           emacs-output-stream start-swank-server write-emacs-command
           simple-server stop-simple-server call-simple-server
           enable-sharpL-reader
           list-threads kill-all-workers thread-backtrace thread-break
           view-in-web-browser
           with-hash-values expiring-hash-table set-expiring-hash
           gethash-expiring
           process-lines
           defun-simple-memoized
           list-hash-keys
           agent start-agent list-agents 
           ensure-string bugout prin1-with-ellipses breakout
           destructuring-bind-list
           symbol-ends-with symbol-start-with
           prologue list-testsuites list-all-tests list-tests 
           describe-test-result
           run-testsuites run-test remove-all-tests test tests
           sg clear-debugging-globals))
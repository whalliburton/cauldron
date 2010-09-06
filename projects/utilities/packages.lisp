;; packages.lisp

(defpackage utilities
  (:use common-lisp iterate split-sequence local-time usocket sb-thread)
  (:import-from sb-ext quit run-program)
  (:import-from alexandria once-only)
  (:import-from sb-gray fundamental-character-output-stream
                stream-write-char stream-force-output)
  (:export newline slurp slurp-stream slurp-lines slurp-line last1 run
           string-starts-with string-ends-with string-contains-p
           chew-string process-string print-table print-heading
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
           with-hash-values))
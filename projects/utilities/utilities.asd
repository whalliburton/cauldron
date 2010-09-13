;; utilities.asd

(defsystem :utilities
  :serial t
  :components ((:static-file "utilities.asd")
               (:file "isaac")
               (:file "string-case")
	       (:file "packages")
	       (:file "first")
	       (:file "random")
	       (:file "slurping")
	       (:file "run")
               (:file "strings")
               (:file "print-table")
               (:file "pretty-printing")
               (:file "time")
               (:file "symbols")
               (:file "assoc")
               (:file "emacs-output-stream")
               (:file "slime")
               (:file "simple-server")
               (:file "sharpl")
               (:file "hash-tables")
               (:file "threads")
               (:file "processes"))
  :depends-on (:iterate :split-sequence :local-time :swank :usocket :iolib.process
                        :fare-utils))



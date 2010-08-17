;; utilities.asd

(defsystem :utilities
  :serial t
  :components ((:static-file "utilities.asd")
	       (:file "packages")
	       (:file "first")
	       (:file "slurping")
	       (:file "run")
               (:file "strings")
               (:file "print-table")
               (:file "pretty-printing")
               (:file "time")
               (:file "symbols")
               (:file "assoc"))
  :depends-on (:iterate :split-sequence :local-time))



;; utilities.asd

(defsystem :utilities
  :serial t
  :components ((:static-file "utilities.asd")
	       (:file "packages")
	       (:file "first")
	       (:file "slurping")
	       (:file "run")
               (:file "strings")
               (:file "pretty-printing"))
  :depends-on (:iterate :split-sequence))



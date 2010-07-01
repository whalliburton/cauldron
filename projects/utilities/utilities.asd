;; utilities.asd

(defsystem :utilities
  :serial t
  :components ((:static-file "utilities.asd")
	       (:file "packages")
	       (:file "first")
	       (:file "slurping")
	       (:file "run")
               (:file "strings"))
  :depends-on (:iterate :split-sequence))



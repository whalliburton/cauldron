;; utilities.asd

(defsystem :utilities
  :serial t
  :components ((:static-file "utilities.asd")
	       (:file "packages")
	       (:file "first")
	       (:file "slurping")
	       (:file "run"))
  :depends-on (:iterate :split-sequence))



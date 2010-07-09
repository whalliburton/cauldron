;; linux.asd

(defsystem :linux
  :serial t
  :components ((:static-file "linux.asd")
	       (:file "packages")
	       (:file "shared-libraries"))
  :depends-on (:utilities))



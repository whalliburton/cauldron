;; linux.asd

(defsystem :linux
  :serial t
  :components ((:file "packages")
	       (:file "shared-libraries"))
  :depends-on (:utilities))



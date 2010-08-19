;; linux.asd

(defsystem :linux
  :serial t
  :components ((:file "packages")
	       (:file "shared-libraries")
               (:file "magic"))
  :depends-on (:utilities))



;; linux.asd

(defsystem :linux
  :serial t
  :components ((:file "packages")
	       (:file "shared-libraries")
               (:file "magic")
               (:file "ping"))
  :depends-on (:utilities))



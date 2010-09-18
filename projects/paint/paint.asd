;; paint.asd

(defsystem :paint
  :serial t
  :components ((:static-file "paint.asd")
	       (:file "packages")
	       (:file "commands")
	       (:file "server")
	       (:file "initialize"))
  :depends-on (:utilities :graphics))



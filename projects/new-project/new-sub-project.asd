;; new-project.asd

(defsystem :new-project
  :serial t
  :components ((:static-file "new-project.asd")
	       (:file "packages")
	       (:file "first"))
  :depends-on ())



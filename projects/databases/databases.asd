;; databases.asd

(defsystem :databases
  :serial t
  :components ((:static-file "databases.asd")
	       (:file "packages")
	       (:file "recipe"))
  :depends-on (:drakma :cl-json :iterate))



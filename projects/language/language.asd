;; language.asd

(defsystem :language
  :serial t
  :components ((:static-file "language.asd")
	       (:file "packages")
	       (:file "translate"))
  :depends-on (:drakma :cl-json :iterate))



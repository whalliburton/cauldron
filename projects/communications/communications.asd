;; communications.asd

(defsystem :communications
  :serial t
  :components ((:static-file "communications.asd")
	       (:file "packages")
	       (:file "bbdb"))
  :depends-on (:named-readtables))



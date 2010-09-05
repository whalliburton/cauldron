;; journal.asd

(defsystem :journal
  :serial t
  :components ((:static-file "journal.asd")
	       (:file "packages")
	       (:file "journal"))
  :depends-on (:local-time :utilities :editor :cl-fad :iterate :chronicity))



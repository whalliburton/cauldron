;; journal.asd

(defsystem :journal
  :serial t
  :components ((:static-file "journal.asd")
	       (:file "packages")
	       (:file "emacs")
	       (:file "journal"))
  :depends-on (:local-time :utilities :cl-fad :iterate :chronicity))



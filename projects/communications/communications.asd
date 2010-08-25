;; communications.asd

(defsystem :communications
  :serial t
  :components ((:static-file "communications.asd")
	       (:file "packages")
	       (:file "bbdb")
	       (:file "mail"))
  :depends-on (:utilities :iterate :named-readtables :cl-smtp
                          :cl-irc :split-sequence))



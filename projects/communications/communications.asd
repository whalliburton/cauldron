;; communications.asd

(defsystem :communications
  :serial t
  :components ((:static-file "communications.asd")
	       (:file "packages")
	       (:file "bbdb")
	       (:file "mail")
               (:file "irc"))
  :depends-on (:utilities :databases 
                          :iterate :named-readtables :cl-smtp
                          :cl-irc :split-sequence))



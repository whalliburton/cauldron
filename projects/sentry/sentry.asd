;; sentry.asd

(defsystem :sentry
  :serial t
  :components ((:static-file "sentry.asd")
	       (:file "packages")
	       (:file "irc-monitor")
               (:file "server")
	       (:file "initialize"))
  :depends-on (:utilities :databases :cl-irc :iterate))

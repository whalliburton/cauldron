;; paint-client.asd

(defsystem :paint-client
  :serial t
  :components ((:static-file "paint-client.asd")
	       (:file "client-packages")
	       (:file "client"))
  :depends-on (:utilities))



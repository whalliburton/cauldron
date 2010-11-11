;; network.asd

(defsystem :network
  :serial t
  :components ((:static-file "network.asd")
               (:file "packages")
               (:file "ec2")
               (:file "ssh")
               (:file "initialize"))
  :depends-on (:utilities :alexandria :ec2 :iterate :local-time :web))
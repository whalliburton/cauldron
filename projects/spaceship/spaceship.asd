;; spaceship.asd

(defsystem :spaceship
  :serial t
  :components ((:file "packages")
	       (:file "files")
               (:file "threads")
               (:file "stumpwm")
               (:file "new-project")
               (:file "database")
               (:file "help")
	       (:file "initialize"))
  :depends-on (
               
               :stumpwm 
               :swank
               :bknr.datastore
               :iterate

               :hardware
               :linux
               :cards 
               :web 
               :music 
               :databases 
               :timer 
               :network 
               :language

               ))



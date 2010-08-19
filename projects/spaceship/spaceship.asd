;; spaceship.asd

(defsystem :spaceship
  :serial t
  :components ((:file "packages")
	       (:file "files")
               (:file "threads")
               (:file "stumpwm")
               (:file "new-project")
               (:file "database")
	       (:file "initialize"))
  :depends-on (
               
               :stumpwm 
               :swank
               :bknr.datastore

               :cards 
               :laptop 
               :web 
               :music 
               :databases 
               :iterate
               :timer 
               :network 
               :language
               :linux

               ))



;; spaceship.asd

(defsystem :spaceship
  :serial t
  :components ((:file "packages")
	       (:file "files")
               (:file "new-project")
               (:file "scanner")
               (:file "help")
               (:file "shell-control")
	       (:file "initialize"))
  :depends-on (
               
               :swank
               :iterate

               :windows
               :hardware
               :linux
               :cards 
               :web 
               :music 
               :databases 
               :timer 
               :network 
               :language
               :journal
               :documents
               :communications
               :paint-client

               ))



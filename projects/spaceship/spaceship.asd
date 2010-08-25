;; spaceship.asd

(defsystem :spaceship
  :serial t
  :components ((:file "packages")
	       (:file "files")
               (:file "threads")
               (:file "new-project")
               (:file "shell-control")
               (:file "help")
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

               ))



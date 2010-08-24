;; spaceship.asd

(defsystem :spaceship
  :serial t
  :components ((:file "packages")
	       (:file "files")
               (:file "threads")
               (:file "stumpwm")
               (:file "new-project")
               (:file "shell-control")
               (:file "help")
	       (:file "initialize"))
  :depends-on (
               
               :stumpwm 
               :swank
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
               :journal
               :documents

               ))



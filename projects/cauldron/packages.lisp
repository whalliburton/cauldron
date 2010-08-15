;; packages.lisp

(defpackage cauldron
  (:use common-lisp cards laptop web music utilities iterate sb-thread databases
        network language
;;        linux
        )
  (:import-from sb-ext quit run-program posix-getenv)
  (:export initialize new-project))



  

	   
	   

	   
	

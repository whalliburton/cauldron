;; packages.lisp

(defpackage spaceship
    (:use common-lisp sb-thread iterate 
          utilities databases cards laptop web music 
          network language linux)
  (:import-from sb-ext quit run-program posix-getenv)
  (:export initialize))



  

	   
	   

	   
	
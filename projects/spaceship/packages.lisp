;; packages.lisp

(defpackage spaceship
    (:use common-lisp sb-thread iterate bknr.datastore alexandria
          utilities databases cards hardware web music 
          network language linux)
  (:import-from sb-ext quit run-program posix-getenv)
  (:export initialize))



  

	   
	   

	   
	

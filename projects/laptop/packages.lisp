;; packages.lisp

(defpackage laptop
  (:use common-lisp utilities split-sequence print-table cl-fad anaphora)
  (:import-from sb-ext quit)
  (:export thermal 
           devices device power-supply battery
           memory-information 
           processes process 
           identities ssh-identity
           system-information))



  

	   
	   

	   
	

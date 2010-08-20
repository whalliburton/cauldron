;; packages.lisp

(defpackage databases
  (:use common-lisp drakma json iterate 
        bknr.datastore bknr.indices bknr.utils
        utilities linux string-case anaphora)
  (:import-from sb-ext quit)
  (:export cached-http-request recipe))

(in-package :databases)

(defparameter *help-text* "Access to online databases.")


  

	   
	   

	   
	

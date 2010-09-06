;; packages.lisp

(defpackage databases
  (:use common-lisp drakma json iterate 
        bknr.datastore bknr.indices bknr.utils
        utilities linux string-case anaphora)
  (:import-from alexandria if-let)
  (:import-from sb-ext quit posix-getenv)
  (:export cached-http-request recipe start-local-database
           gethash-database add-idea list-ideas
           create-blob-link weather))

(in-package :databases)

(defparameter *help-text* "Access to online and local databases.")


  

	   
	   

	   
	

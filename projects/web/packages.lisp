;; packages.lisp

(defpackage web
  (:use common-lisp)
  (:import-from sb-ext quit run-program)
  (:export web sr google javadoc rfc rhyme thesaurus wayback))

(in-package :web)

(defparameter *help-text* "Tools for accessing the World Wide Web.")


  

	   
	   

	   
	

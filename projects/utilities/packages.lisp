;; packages.lisp

(defpackage utilities
  (:use common-lisp iterate split-sequence)
  (:import-from sb-ext quit run-program)
  (:export newline slurp slurp-stream slurp-lines slurp-and-split-on-colon last1 run
           string-starts-with chew-string process-string print-table print-heading))



  

	   
	   

	   
	

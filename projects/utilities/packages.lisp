;; packages.lisp

(defpackage utilities
  (:use common-lisp iterate split-sequence local-time)
  (:import-from sb-ext quit run-program)
  (:export newline slurp slurp-stream slurp-lines slurp-line last1 run
           string-starts-with string-contains-p
           chew-string process-string print-table print-heading
           seconds-to-duration-string with-assoc
           mkstr symb ksymb in-home
           random-string random-byte-vector))



  

	   
	   

	   
	

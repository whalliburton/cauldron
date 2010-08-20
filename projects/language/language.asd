;; language.asd

(defsystem :language
  :serial t
  :components ((:static-file "language.asd")
	       (:file "packages")
	       (:file "translate")
               (:file "festival"))
  :depends-on (:drakma :cl-json :iterate :usocket 
                       :babel :utilities :anaphora))




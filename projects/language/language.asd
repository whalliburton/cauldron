;; language.asd

(defsystem :language
  :serial t
  :components ((:static-file "language.asd")
	       (:file "packages")
	       (:file "translate")
               (:file "festival")
               (:file "iso-639"))
  :depends-on (:drakma :cl-json :iterate :usocket 
                       :babel :utilities :anaphora))




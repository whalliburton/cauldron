;; language.asd

(defsystem :language
  :serial t
  :components ((:static-file "language.asd")
	       (:file "packages")
	       (:file "translate")
               (:file "festival")
               (:file "iso-639")
               (:file "shtooka"))
  :depends-on (:drakma :cl-json :iterate :usocket 
                       :babel :utilities :anaphora
                       :bknr.datastore :bknr.indices
                       :databases))




;; documents.asd

(defsystem :documents
  :serial t
  :components ((:static-file "documents.asd")
	       (:file "packages")
	       (:file "read"))
  :depends-on (:databases :iterate :split-sequence))



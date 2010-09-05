;; documents.asd

(defsystem :documents
  :serial t
  :components ((:static-file "documents.asd")
	       (:file "packages")
	       (:file "read")
	       (:file "open"))
  :depends-on (:databases :iterate :split-sequence :utilities :editor :torrents
                          :iolib.process :cl-bzip2))

;; documents.asd

(defsystem :documents
  :serial t
  :components ((:static-file "documents.asd")
	       (:file "packages")
	       (:file "read")
	       (:file "open")
               (:file "torrent"))
  :depends-on (:databases :iterate :split-sequence :utilities :editor
                          :iolib.process :cl-bzip2 :gzip-stream :drakma :ironclad
                          :flexi-streams :bencode))

;; databases.asd

(defsystem :databases
  :serial t
  :components ((:static-file "databases.asd")
	       (:file "packages")
               (:file "cached-http-request")
	       (:file "recipe"))
  :depends-on (:drakma :cl-json :iterate 
                       :bkrn.datastore :bknr.indices :bknr.utils))



;; databases.asd

(defsystem :databases
  :serial t
  :components ((:static-file "databases.asd")
	       (:file "packages")
	       (:file "local")
               (:file "cached-http-request")
	       (:file "recipe")
               (:file "ideas")
               (:file "weather"))
  :depends-on (:drakma :cl-json :iterate :alexandria
                       :bknr.datastore :bknr.indices :bknr.utils
                       :utilities :linux :anaphora))



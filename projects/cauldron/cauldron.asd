;; cauldron.asd

(defsystem :cauldron
  :serial t
  :components ((:static-file "cauldron.asd")
	       (:file "packages")
	       (:file "first")
               (:file "threads")
	       (:file "initialize"))
  :depends-on (:cards :laptop :web :music :databases :print-table :iterate
                      :timer))



;; print-table.asd

(defsystem :print-table
  :serial t
  :components ((:static-file "print-table.asd")
	       (:file "packages")
	       (:file "print-table"))
  :depends-on (:utilities :iterate))



;; laptop.asd

(defsystem :laptop
  :serial t
  :components ((:static-file "laptop.asd")
	       (:file "packages")
	       (:file "thinkpad"))
  :depends-on (:utilities :print-table :split-sequence))



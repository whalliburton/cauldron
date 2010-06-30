;; laptop.asd

(defsystem :laptop
  :serial t
  :components ((:static-file "laptop.asd")
	       (:file "packages")
	       (:file "thinkpad")
	       (:file "devices")
               (:file "memory")
               (:file "processes")
               (:file "system"))
  :depends-on (:utilities :print-table :split-sequence :cl-fad :anaphora))



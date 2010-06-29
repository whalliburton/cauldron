;; laptop.asd

(defsystem :laptop
  :serial t
  :components ((:static-file "laptop.asd")
	       (:file "packages")
	       (:file "thinkpad")
	       (:file "devices"))
  :depends-on (:utilities :print-table :split-sequence :cl-fad))



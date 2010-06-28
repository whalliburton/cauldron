;; cauldron.asd

(defsystem :cauldron
  :serial t
  :components ((:static-file "cauldron.asd")
	       (:file "packages")
	       (:file "first")
	       (:file "initialize"))
  :depends-on (:cards :laptop :web))



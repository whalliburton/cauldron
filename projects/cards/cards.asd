;; cards.asd

(defsystem :cards
  :serial t
  :components ((:static-file "cards.asd")
	       (:file "packages")
	       (:file "first")
	       (:file "initialize"))
  :depends-on (:local-time))



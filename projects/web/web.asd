;; web.asd

(defsystem :web
  :serial t
  :components ((:static-file "web.asd")
	       (:file "packages")
	       (:file "web")
	       (:file "surfraw"))
  :depends-on ())



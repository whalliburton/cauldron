;; editor.asd

(defsystem :editor
  :serial t
  :components ((:static-file "editor.asd")
	       (:file "packages")
	       (:file "emacs"))
  :depends-on (:utilities))



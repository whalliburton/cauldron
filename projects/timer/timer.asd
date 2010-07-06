;; timer.asd

(defsystem :timer
  :serial t
  :components ((:static-file "timer.asd")
	       (:file "packages")
	       (:file "timer"))
  :depends-on (:graphics :local-time))



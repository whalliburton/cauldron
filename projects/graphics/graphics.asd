;; graphics.asd

(defsystem :graphics
  :serial t
  :components ((:static-file "graphics.asd")
	       (:file "packages")
	       (:file "window"))
  :depends-on (:utilities :cl-cairo2 :cl-cairo2-xlib :cl-colors))



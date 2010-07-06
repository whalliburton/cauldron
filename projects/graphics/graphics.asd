;; graphics.asd

(defsystem :graphics
  :serial t
  :components ((:static-file "graphics.asd")
	       (:file "packages")
               (:file "constants")
               (:file "xlib")
	       (:file "window")
               (:file "demo"))
  :depends-on (:utilities :cl-cairo2 :cl-cairo2-xlib :cl-colors))



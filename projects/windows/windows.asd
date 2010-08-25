;; windows.asd

(defsystem :windows
  :serial t
  :components ((:static-file "windows.asd")
	       (:file "packages")
	       (:file "stumpwm")
               (:file "windows"))
  :depends-on (:swank :stumpwm :iterate :utilities))



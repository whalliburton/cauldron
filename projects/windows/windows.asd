;; windows.asd

(defsystem :windows
  :serial t
  :components ((:static-file "windows.asd")
	       (:file "packages")
	       (:file "stumpwm"))
  :depends-on (:swank :stumpwm))



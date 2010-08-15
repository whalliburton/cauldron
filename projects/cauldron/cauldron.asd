;; cauldron.asd

(defsystem :cauldron
  :serial t
  :components ((:static-file "cauldron.asd")
	       (:file "packages")
	       (:file "files")
               (:file "threads")
               (:file "stumpwm")
               (:file "new-project")
	       (:file "initialize"))
  :depends-on (:stumpwm :swank
               :cards :laptop :web :music :databases :iterate
               :timer :network :language
               ;;                      :linux
               ))



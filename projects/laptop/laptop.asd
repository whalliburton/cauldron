;; laptop.asd

(defsystem :laptop
  :serial t
  :components ((:static-file "laptop.asd")
	       (:file "packages")
	       (:file "thinkpad")
	       (:file "devices")
               (:file "memory")
               (:file "processes")
               (:file "system")
               (:file "udev")
               ;;(:file "wireless")
               )
  :depends-on (:utilities :split-sequence :cl-fad :anaphora :cffi :iterate
                          :sb-concurrency :alexandria))



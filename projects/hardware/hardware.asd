;; hardware.asd

(defsystem :hardware
  :serial t
  :components ((:static-file "hardware.asd")
               (:file "package")
               (:file "thinkpad")
               (:file "devices")
               (:file "memory")
               (:file "processes")
               (:file "monitor")
               (:file "system")
               (:file "udev")
               (:file "cpu")
               ;;(:file "wireless")
               )
  :depends-on (:utilities :split-sequence :cl-fad :anaphora :cffi :iterate
                          :sb-concurrency :alexandria))



;; manual.asd

(defsystem :manual
  :serial t
  :components ((:static-file "manual.asd")
               (:file "docstrings")
               (:file "packages")
               (:file "build-manual"))
  :depends-on ())



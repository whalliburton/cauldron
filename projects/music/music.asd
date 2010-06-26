;; music.asd

(defsystem :music
  :serial t
  :components ((:static-file "music.asd")
	       (:file "packages")
	       (:file "music"))
  :depends-on (:sb-concurrency))



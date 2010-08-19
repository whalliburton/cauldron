;; music.asd

(defsystem :music
  :serial t
  :components ((:static-file "music.asd")
	       (:file "packages")
	       (:file "midi")
	       (:file "play"))
  :depends-on (:sb-concurrency))



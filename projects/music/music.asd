;; music.asd

(defsystem :music
  :serial t
  :components ((:static-file "music.asd")
	       (:file "packages")
	       (:file "midi")
	       (:file "ogg")
	       (:file "play")
               (:file "initialize"))
  :depends-on (:sb-concurrency :vorbisfile-ffi :mixalot))



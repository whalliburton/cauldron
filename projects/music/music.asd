;; music.asd

(defsystem :music
  :serial t
  :components ((:static-file "music.asd")
	       (:file "packages")
               (:file "wav")
	       (:file "play-midi")
	       (:file "ogg")
	       (:file "play")
               (:file "freesound")
               (:file "alsa-mixer")
               (:file "noise")
               (:file "octaves")
               (:file "initialize"))
  :depends-on (:linux 
               :sb-concurrency 
               :iterate 
               :mixalot :mixalot-vorbis :vorbisfile-ffi
               :cl-web-utils
               :databases
               :cffi))



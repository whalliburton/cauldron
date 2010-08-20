;; music.asd

(defsystem :music
  :serial t
  :components ((:static-file "music.asd")
	       (:file "packages")
	       (:file "midi")
	       (:file "ogg")
	       (:file "play")
               (:file "freesound")
               (:file "initialize"))
  :depends-on (:linux 
               :sb-concurrency 
               :iterate 
               :mixalot :mixalot-vorbis :vorbisfile-ffi
               :cl-web-utils
               :databases))



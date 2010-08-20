;; packages.lisp

(defpackage music
  (:use common-lisp sb-concurrency sb-thread iterate)
  (:import-from sb-ext quit run-program process-kill process-alive-p process-input)
  (:import-from linux magic-mime)
  (:import-from alexandria when-let)
  (:import-from mixalot main-thread-init create-mixer destroy-mixer mixer-add-streamer
                streamer-cleanup)
  (:import-from vorbisfile
                vorbis-file vorbis-open vorbis-close 
                get-vorbis-rate
                get-vorbis-comment get-vorbis-raw-tags-from-comment)
  (:import-from mixalot-vorbis make-vorbis-streamer vorbis-streamer)
  (:export *currently-playing* initialize-music play stop))

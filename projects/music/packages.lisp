;; packages.lisp

(defpackage :wav
  (:use :common-lisp)
  (:import-from mixalot create-mixer destroy-mixer mixer-add-streamer streamer-cleanup
                make-fast-vector-streamer-mono
                make-fast-vector-streamer-interleaved-stereo)
  (:export read-wav play-wav))

(defpackage music
  (:use common-lisp sb-concurrency sb-thread iterate databases bknr.datastore
        drakma cl-web-utils databases utilities wav cffi)
  (:import-from sb-ext quit run-program process-kill process-alive-p process-input)
  (:import-from linux magic-mime)
  (:import-from alexandria when-let with-gensyms)
  (:import-from mixalot main-thread-init create-mixer destroy-mixer mixer-add-streamer
                streamer-cleanup)
  (:import-from vorbisfile
                vorbis-file vorbis-open vorbis-close 
                get-vorbis-rate
                get-vorbis-comment get-vorbis-raw-tags-from-comment)
  (:import-from mixalot-vorbis make-vorbis-streamer vorbis-streamer)
  (:export *currently-playing* initialize-music play stop
           freesound-search freesound-download mixer
           play-note play-notes))

(in-package :music)

(defparameter *help-text* "Play music.")
(defvar *currently-playing* nil)

(enable-sharpl-reader)


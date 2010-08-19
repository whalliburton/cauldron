;; packages.lisp

(defpackage music
  (:use common-lisp sb-concurrency sb-thread)
  (:import-from sb-ext quit run-program process-kill process-alive-p process-input)
  (:import-from linux magic-mime)
  (:export *help-text* *currently-playing* play stop))

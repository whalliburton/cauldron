;; packages.lisp

(defpackage language
  (:use common-lisp drakma json iterate usocket babel utilities string-case
        anaphora)
  (:import-from sb-ext quit run-program process-status process-kill)
  (:import-from flexi-streams with-output-to-sequence)
  (:export translate say speak-in-english speak-in-russian))

(in-package :language)

(defparameter *help-text* "Spoken and written language tools.")
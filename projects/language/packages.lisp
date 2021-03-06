;; packages.lisp

(defpackage language
  (:use common-lisp drakma json iterate usocket babel utilities string-case
        anaphora databases bknr.datastore bknr.indices split-sequence)
  (:import-from sb-ext quit run-program process-status process-kill)
  (:import-from flexi-streams with-output-to-sequence)
  (:import-from alexandria when-let)
  (:export translate say speak-in-english speak-in-russian
           language-name-from-code shtooka-word))

(in-package :language)

(defparameter *help-text* "Spoken and written language tools.")
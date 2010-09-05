;; packages.lisp

(defpackage journal
  (:use common-lisp local-time utilities cl-fad iterate editor)
  (:import-from sb-ext quit run-program)
  (:export journal))

(in-package :journal)

(defparameter *help-text* "Journal writing support.")
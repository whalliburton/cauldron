;; packages.lisp

(defpackage documents
  (:use common-lisp iterate split-sequence bknr.datastore bknr.indices
        utilities linux string-case editor databases iolib.process
        drakma flexi-streams)
  (:import-from sb-ext quit run-program)
  (:import-from alexandria if-let)
  (:export list-documents read-document open-document get-document))

(in-package :documents)

(defparameter *help-text* "The reading library.")

;; packages.lisp

(defpackage communications
  (:use common-lisp utilities iterate named-readtables cl-smtp split-sequence)
  (:import-from sb-ext quit)
  (:import-from alexandria shuffle if-let)
  (:export load-email-identity contacts mail))

(in-package :communications)

(defparameter *help-text* "Communicating with other people.")
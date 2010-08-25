;; packages.lisp

(defpackage communications
  (:use common-lisp utilities iterate named-readtables cl-smtp)
  (:import-from sb-ext quit)
  (:export load-email-identity contacts mail))

(in-package :communications)

(defparameter *help-text* "Communicating with other people.")
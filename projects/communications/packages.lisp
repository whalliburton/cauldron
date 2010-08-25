;; packages.lisp

(defpackage communications
  (:use common-lisp utilities iterate named-readtables)
  (:import-from sb-ext quit)
  (:export contacts))

(in-package :communications)

(defparameter *help-text* "Communicating with other people.")
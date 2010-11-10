;; packages.lisp

(defpackage communications
  (:use common-lisp utilities databases
        iterate named-readtables cl-smtp split-sequence
        bknr.datastore iolib.process)
  (:import-from sb-ext quit)
  (:import-from alexandria shuffle if-let)
  (:export load-email-identity contacts mail latest-irc network ids
           scp failure-to-scp))

(in-package :communications)

(defparameter *help-text* "Communicating with other people.")
(enable-sharpl-reader)
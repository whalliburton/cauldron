;; packages.lisp

(defpackage network
  (:use common-lisp utilities ec2 iterate local-time split-sequence)
  (:import-from web web)
  (:import-from sb-ext quit run-program)
  (:import-from alexandria with-gensyms when-let if-let once-only)
  (:export instances i
           run-instance run
           terminate-instance terminate
           view-instance view
           images set-aws-keys ssh-command))

(in-package :network)

(defparameter *help-text* "Inspect remote systems.")
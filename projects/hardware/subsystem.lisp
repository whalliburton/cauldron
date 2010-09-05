;; subsystem.lisp

(defpackage hardware
  (:use common-lisp utilities split-sequence cl-fad anaphora cffi iterate
        sb-thread sb-concurrency)
  (:import-from sb-ext quit)
  (:import-from alexandria flatten)
  (:export thermal 
           devices device power-supply battery input
           memory-information 
           processes process describe-process
           identities ssh-identity
           system
           start-udev-monitor
           print-pending-udev-messages))

(in-package :hardware)

(defparameter *help-text* "Hardware inspection utilities.")

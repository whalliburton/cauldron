;; packages.lisp

(defpackage laptop
  (:use common-lisp utilities split-sequence cl-fad anaphora cffi iterate
        sb-thread sb-concurrency)
  (:import-from sb-ext quit)
  (:import-from alexandria flatten)
  (:export thermal 
           devices device power-supply battery input
           memory-information 
           processes process 
           identities ssh-identity
           system-information
           start-udev-monitor
           ))

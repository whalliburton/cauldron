;; initialize.lisp

(in-package :network)

(defparameter *network-keys-path* "/home/conrad/private/ec2-keys.lisp")

(when (probe-file *network-keys-path*)
  (load *network-keys-path*))

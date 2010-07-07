;; packages.lisp

(defpackage network
  (:use common-lisp utilities ec2 iterate local-time)
  (:import-from sb-ext quit run-program)
  (:import-from alexandria with-gensyms)
  (:export instances set-aws-keys ssh-command))

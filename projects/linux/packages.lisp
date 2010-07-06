;; packages.lisp

(defpackage linux
  (:use common-lisp split-sequence utilities)
  (:import-from sb-ext quit run-program process-exit-code)
  (:export list-shared-libraries failure-to-list-shared-libraries))

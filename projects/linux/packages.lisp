;; packages.lisp

(defpackage linux
  (:use common-lisp utilities)
  (:import-from sb-ext quit run-program process-exit-code)
  (:import-from alexandria when-let)
  (:export list-shared-libraries failure-to-list-shared-libraries magic-mime
           ping))

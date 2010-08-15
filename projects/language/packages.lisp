;; packages.lisp

(defpackage language
  (:use common-lisp drakma json iterate)
  (:import-from sb-ext quit)
  (:export translate))

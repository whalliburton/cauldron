;; packages.lisp

(defpackage editor
  (:use common-lisp utilities)
  (:import-from sb-ext quit)
  (:export open-in-emacs view-in-emacs view-in-emacs-w3m))

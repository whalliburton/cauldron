;; packages.lisp

(defpackage paint
  (:use common-lisp utilities cl-colors cl-cairo2 graphics)
  (:import-from sb-ext quit schedule-timer make-timer))

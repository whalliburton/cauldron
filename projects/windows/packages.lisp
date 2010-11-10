;; packages.lisp

(defpackage windows
  (:use common-lisp stumpwm iterate utilities)
  (:import-from sb-ext quit)
  (:export list-windows destroy-window show-window))

(in-package :windows)

(defparameter *help-text* "Desktop window tools.")
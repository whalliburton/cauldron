;; emacs.lisp

(in-package :utilities)

(defun open-in-emacs (filename)
  (run-program "/usr/bin/emacsclient" (list "--no-wait" filename)))


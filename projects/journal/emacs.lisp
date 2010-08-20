;; emacs.lisp

(in-package :journal)

(defun open-in-emacs (filename)
  (run-program "/usr/bin/emacsclient" (list "--no-wait" filename)))



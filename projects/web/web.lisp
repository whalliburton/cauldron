;; web.lisp

(in-package :web)

(defun web (url)
  "Fire up a web browser viewing URL."
  (run-program "/usr/bin/chromium" (list url)))

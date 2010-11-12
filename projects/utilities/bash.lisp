;; bash.lisp

(in-package :utilities)

(defmacro bash-string (body &rest arguments)
  `(format nil ,(concatenate 'string
                             (format nil "#!/bin/bash~%~%")
                             body)
           ,@arguments))

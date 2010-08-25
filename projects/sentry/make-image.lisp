(require 'asdf)
(pushnew "/lisp/projects/systems/" asdf:*central-registry* :test #'string=)
(require :sb-posix)
(require :sentry)
(sb-ext:save-lisp-and-die 
 "sentry" 
 :toplevel (lambda ()
             ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
             (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
             (sentry:initialize)
             0)
 :executable t)

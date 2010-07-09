(require 'asdf)
(pushnew "/lisp/projects/systems/" asdf:*central-registry* :test #'string=)
(require :cauldron)
(sb-ext:save-lisp-and-die 
 "cauldron" 
 :toplevel (lambda ()
             ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
             (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
             (cauldron:initialize)
             (stumpwm:stumpwm)
             0)
 :executable t)

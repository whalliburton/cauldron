(require 'asdf)
(pushnew "/lisp/projects/systems/" asdf:*central-registry* :test #'string=)
(require :spaceship)
(sb-ext:save-lisp-and-die 
 "spaceship" 
 :toplevel (lambda ()
             ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
             (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
             (spaceship::initialize)
             (stumpwm:stumpwm)
             0)
 :executable t)

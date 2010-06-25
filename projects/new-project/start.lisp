
(require 'asdf)

(setf asdf:*central-registry* 
      '("/lisp/projects/systems/" "/lisp/site-lisp/systems/")) 

(require 'new-project)
(in-package :new-project)

(require 'swank)

(loop with running = t
      for port = 4012 then (1+ port)
      while running
      do 
   (handler-case 
       (progn
         (format t "Starting SWANK on ~a~%" port)
         (swank:create-server :dont-close t :port port :coding-system "utf-8-unix")
         (setf running nil)
         (sb-bsd-sockets:address-in-use-error () nil))))

(new-project::initialize)


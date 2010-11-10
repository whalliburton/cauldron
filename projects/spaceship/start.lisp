
(require 'asdf)

(setf asdf:*central-registry*
      '("/lisp/projects/systems/" "/lisp/site-lisp/systems/"))

(require 'spaceship)
(in-package :spaceship)

(utilities:start-swank-server)

(spaceship::initialize)


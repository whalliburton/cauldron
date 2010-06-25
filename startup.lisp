;; startup.lisp

(pushnew "/lisp/projects/systems/" asdf:*central-registry* :test #'string=)

(require :cauldron)
(in-package :cauldron)


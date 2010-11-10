;; new-project.lisp

(in-package :spaceship)

(defun new-project (name)
  (run-program "/lisp/projects/new-project/new_sub_project"
               (list (string-downcase name)))
  (require (intern (string-upcase name) :keyword)))


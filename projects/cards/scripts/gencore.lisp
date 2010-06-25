;; gencore.lisp

(require 'asdf)

(setf asdf:*central-registry* 
      '("/mnt/projects/site-lisp/systems/" "/mnt/projects/systems/")) 

(require 'swank)

(dolist (m (cdadar (slot-value (asdf:find-system 'cards) 'asdf::do-first)))
  (unless (member m '()) (require m)))

(save-lisp-and-die "/mnt/cores/cards.core")



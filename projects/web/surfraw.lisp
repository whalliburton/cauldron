;; surfraw.lisp

(in-package :web)

(defun surfraw (args)
  (run-program "/usr/bin/surfraw" args))

(defmacro sr (elviname &rest terms)
  `(surfraw (list ,(format nil "~(~a~)" elviname) ,(format nil "~{~(~a~)~^ ~}" terms))))

(defmacro google (&rest terms)
  "Google search."
  `(sr google ,@terms))

(defmacro javadoc (&rest terms)
  "Javadoc search."
  `(sr javasun ,@terms))

(defmacro rfc (&rest terms)
  "RFC search."
  `(sr rfc ,@terms))

(defmacro rhyme (&rest terms)
  "Rhyming word search."
  `(sr rhyme ,@terms))

(defmacro thesaurus (&rest terms)
  "Thesaurus search."
  `(sr thesaurus ,@terms))

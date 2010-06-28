;; surfraw.lisp

(in-package :web)

(defun surfraw (args)
  (run-program "/usr/bin/surfraw" args))

(defmacro sr (elviname &rest terms)
  `(surfraw (list ,(format nil "~(~a~)" elviname) ,(format nil "~{~(~a~)~^ ~}" terms))))

(defmacro google (&rest terms)
  `(sr google ,@terms))

(defmacro javadoc (&rest terms)
  `(sr javasun ,@terms))

(defmacro rfc (&rest terms)
  `(sr rfc ,@terms))

(defmacro rhyme (&rest terms)
  `(sr rhyme ,@terms))

(defmacro thesaurus (&rest terms)
  `(sr thesaurus ,@terms))

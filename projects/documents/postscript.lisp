;; postscript.lisp

(in-package :documents)

(defparameter *postscript-reader* "/usr/bin/gv")

(defclass postscript (base-document)
  ()
  (:metaclass persistent-class))

(defmethod import-document (filename (type (eql :|application/postscript|)))
  (let ((document
         (make-object 'postscript :filename (file-namestring filename))))
    (blob-from-file document filename)
    document))

(defmethod read-document ((document postscript))
  (run-program *postscript-reader* (list (namestring (blob-pathname document))) :wait nil))



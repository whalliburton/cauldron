;; assoc.lisp

(in-package :utilities)

(defmacro with-assoc (fields &body body)
  "Set up keyword accessors for assoc lists."
  `(flet ,(iter (for field in fields) 
                (collect `(,(ksymb field) (assoc) 
                            (cdr (assoc ,(ksymb field) assoc)))))
     ,@body))

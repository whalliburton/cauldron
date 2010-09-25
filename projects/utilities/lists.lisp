;; lists.lisp

(in-package :utilities)

(defmacro destructuring-bind-list (binding-list expression &body body)
  `(iter (for ,binding-list in ,expression)
         (collect ,@body)))


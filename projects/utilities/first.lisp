;; first.lisp

(in-package :utilities)

(defun newline (&rest args)
  (if (integerp (car args))
    (iter (repeat (car args)) (apply #'terpri (cdr args)))
    (apply #'terpri args)))



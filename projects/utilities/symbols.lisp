;; symbols.lisp

(in-package :utilities)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun ksymb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun symbol-ends-with (symbol suffix &key (test #'char-equal))
  "Return T if SYMBOL ends with SUFFIX tested with TEST. Suffix can be
a string or symbol."
  (string-ends-with (symbol-name symbol) (ensure-string suffix) :test test))

(defun symbol-starts-with (symbol prefix &key (test #'char-equal))
  "Return T if SYMBOL starts with SUFFIX tested with TEST. Suffix can
be a string or symbol."
  (string-starts-with (symbol-name symbol) (ensure-string prefix) :test test))

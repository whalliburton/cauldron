;; memoization.lisp

(in-package :utilities)

(defmacro defun-simple-memoized (name (arg &key key (test 'eql) timeout) &body body)
  "Defines a function with name NAME and single arguement ARG that
memoizes its return values in a synchronized hash table with test
TEST. If KEY is set to a function, then KEY is used on ARG to generate
the hash key to memoize. The NAME symbol MEMOIZE-HASH property is set
the hash for inspection. If TIMEOUT is set, use an EXPIRING-HASH-TABLE
with a timeout of TIMEOUT."
  (with-gensyms (hash key-val)
    (if timeout
      `(let ((,hash (make-instance 'expiring-hash-table :timeout ,timeout :test ',test)))
         (defun ,name (,arg) 
           (let ((,key-val ,(if key `(funcall ',key ,arg) `,arg)))
             (values-list
              (or (gethash-expiring ,key-val ,hash)
                  (set-expiring-hash ,hash ,key-val
                                     (multiple-value-list (progn ,@body)))))))
         (setf (get ',name 'memoize-hash) ,hash))
      `(let ((,hash (make-hash-table :synchronized t :test ',test)))
         (defun ,name (,arg) 
           (let ((,key-val ,(if key `(funcall ',key ,arg) `,arg)))
             (values-list
              (or (gethash ,key-val ,hash)
                  (setf (gethash ,key-val ,hash)
                        (multiple-value-list (progn ,@body)))))))
         (setf (get ',name 'memoize-hash) ,hash)))))

(defun-simple-memoized foo (a :test equal :timeout 60) (values a (+ a 2)) )
;; identity.lisp

(in-package :hardware)

(defclass ssh-identity ()
  ((key-size :initarg :key-size)
   (fingerprint :initarg :fingerprint)
   (filename :initarg :filename)
   (type :initarg :type)))

(defmethod print-object ((ssh-identity ssh-identity) stream)
  (with-slots (type filename) ssh-identity
    (print-unreadable-object (ssh-identity stream :type t)
      (format stream "~a ~a" type filename))))

(defun identities ()
  (mapcar (lambda (el)
            (destructuring-bind (size fingerprint filename type) el
              (make-instance 'ssh-identity
                             :key-size size :fingerprint fingerprint :filename filename :type type)))
          (mapcar (lambda (el) (split-sequence #\space el))
                  (split-sequence '#\newline (run-to-string "/usr/bin/ssh-add" "-l")
                                  :remove-empty-subseqs t))))

;; ssh.lisp

(in-package :network)

(defun ssh-command (ip-or-ips command)
  (typecase ip-or-ips
    (string
       (print-heading (format nil "~a ~a" ip-or-ips command))
       (princ (with-output-to-string (str) 
                (run-program "/usr/bin/ssh" (list ip-or-ips command) :output str))))
    (cons 
       (mapc (lambda (el) (ssh-command el command) (newline)) ip-or-ips)))
  nil)

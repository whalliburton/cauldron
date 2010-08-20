;; ssh.lisp

(in-package :network)

(defparameter *watched-ips* nil)

(defun ssh-command (command &optional (ip-or-ips *watched-ips*))
  "Execute COMMAND on IP-OR-IPS."
  (typecase ip-or-ips
    (string
       (print-heading (format nil "~a ~a" ip-or-ips command))
       (princ (with-output-to-string (str) 
                (run-program "/usr/bin/ssh" (list ip-or-ips command) :output str))))
    (cons 
       (mapc (lambda (el) (ssh-command command el) (newline)) ip-or-ips)))
  nil)

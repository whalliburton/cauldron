;; ssh.lisp

(in-package :network)

(defvar *ssh-ips* nil)

(defparameter *ssh-options* "-o StrictHostKeyChecking=no -A")

(defun ssh-command-collect (command ip &key (username "root"))
  (let ((rtn))
    (iterate-process-lines (line (format nil "/usr/bin/ssh ~A -l ~A ~A ~S"
                                         *ssh-options*
                                         username ip command))
      (push line rtn))
    (nreverse rtn)))

(defun ssh-command (command &optional (ip-or-ips *ssh-ips*) (username "root"))
  "Execute COMMAND on IP-OR-IPS."
  (typecase ip-or-ips
    (string
       (print-heading (format nil "executing on ~A@~A ~A" username ip-or-ips command))
       (iterate-process-lines (line
                               (format nil "/usr/bin/ssh ~A -l ~A ~A ~S"
                                       *ssh-options*
                                       username ip-or-ips command))
         (format t "  ~A~%" line)))
    (cons
       (mapc (lambda (el) (ssh-command command el) (newline)) ip-or-ips)))
  (values))

(defparameter *scp-options* "-o StrictHostKeyChecking=no")

(defun scp (from to)
  "SCP from FROM to TO."
  (process-lines (format nil "/usr/bin/scp ~A ~A ~A" *scp-options* from to)))

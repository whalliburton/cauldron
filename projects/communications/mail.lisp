;; mail.lisp

(in-package :communications)

(defparameter *smtp-host* "smtp.gmail.com")
(defparameter *smtp-port* 587)
(defparameter *smtp-ssl* :starttls)

(defvar *smtp-id* nil)
(defvar *smtp-username* nil)
(defvar *smtp-password* nil)

(defparameter *smtp-identity-file* (in-home "/private/smtp-identity.lisp"))

(defun load-email-identity ()
  (when (probe-file *smtp-identity-file*)
    (load *smtp-identity-file*)))

(defun mail (who subject message)
  "Mail WHO an email with SUBJECT and MESSAGE."
  (unless *smtp-id* (error "No email identity has been set."))
  (send-email *smtp-host* *smtp-id* (find-contact who) subject message :port *smtp-port*
              :ssl *smtp-ssl*
              :authentication (and *smtp-username* (list *smtp-username* *smtp-password*))))


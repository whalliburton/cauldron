;; server.lisp

(in-package :sentry)

(defvar *sentry-server*)

(defun start-server ()
  (setf *sentry-server* 
        (make-instance 'simple-server :handler 'sentry-server-handler)))

(defun sentry-server-handler (since)
  (iter (for irc-message in 
             (sort 
              (delete-if (lambda (el) 
                           (and since 
                                ;; this '=' could possibly result in
                                ;; missed messages but duplicates at
                                ;; the margin every time are more
                                ;; annoying to me
                                (>= since (received-time el))))
                         (store-objects-with-class 'irc-message))
              #'< :key 'received-time))
        (with-slots (type received-time source target message) irc-message
          (collect (list type received-time source target message)))))

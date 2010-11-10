;; irc-monitor.lisp

(in-package :sentry)

(defparameter *monitoring-channels* '("#lisp"))

(defclass irc-message (store-object)
  ((received-time :initarg :received-time :reader received-time)
   (type :initarg :type)
   (source :initarg :source)
   (target :initarg :target)
   (message :initarg :message))
  (:metaclass persistent-class))

(defmethod print-object ((obj irc-message) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (type source target message) obj
        (format stream "~A ~A ~A ~A" type source target message))))

(defvar *irc-connection* nil)
(defvar *irc-monitor-thread* nil)

(defun start-irc-monitor ()
  (setf *irc-monitor-thread* (make-thread 'irc-monitor-thread :name "irc-monitor")))

(defun connect-to-irc ()
  (setf *irc-connection* (cl-irc:connect :nickname "sentry"))
  (iter (for channel in *monitoring-channels*)
        (cl-irc:join *irc-connection* channel))
  (start-irc-monitor))

(defun irc-monitor-thread ()
  (iter
    (while (cl-irc::connectedp *irc-connection*))
    (for message = (cl-irc::read-irc-message *irc-connection*))
    (typecase message
      ((or irc-privmsg-message ctcp-action-message)
         (make-object 'irc-message
                      :type (typecase message
                              (irc-privmsg-message :privmsg)
                              (ctcp-action-message :action))
                      :received-time (cl-irc:received-time message)
                      :source (cl-irc:source message)
                      :target (first (cl-irc:arguments message))
                      :message (let ((base (second (cl-irc:arguments message))))
                                 (typecase message
                                   (irc-privmsg-message base)
                                   (ctcp-action-message (subseq base 8 (1- (length base))))))))
      (irc-ping-message
         (cl-irc:pong *irc-connection* (first (cl-irc:arguments message))))
      (t (describe message)))))

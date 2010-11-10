;; irc.lisp

(in-package :communications)

(defvar *irc-connection* nil)
(defvar *irc-nickname* "spaceship")
(defvar *irc-password* nil)

(defvar *sentry-hostname* "localhost")
(defvar *sentry-port* 5020)

(defun connect-to-irc ()
  (setf *irc-connection* (cl-irc:connect :nickname *irc-nickname*)))

(defparameter *colors*
  '("black" "blue" "red" "green" "yellow" "magenta" "cyan" "white")
  "Emacs colors for handle highlighting.")

(defparameter *watching-background*  "red" "The background color for watched handles.")

(defun create-handle-faces ()
  "Return a list of all the possible handle faces."
  (shuffle
   (loop for bg in *colors*
         when (not (equal bg *watching-background*))
           nconc (loop for fg in *colors*
                       when (and (not (equal fg bg))
                                 (not (and (equal fg "white")
                                           (equal bg "black"))))
                         nconc
                         (list (list :foreground fg :background bg))))))

(defvar *watching-handles* '(("minion" "white"))
  "A list of handles to highlight with *watching-background* and their
respective foreground color.")

(defvar *handle-faces* (make-hash-table :test #'equal)
  "The mapping between handles and faces.")

(defvar *faces* (create-handle-faces)
  "The faces available for use.")

(defvar *chats-stream* (make-instance 'emacs-output-stream :name "*chats*")
  "The stream to output the chat.")

(defun propertize (str properties)
  `(propertize ,str 'font-lock-face ',properties))

(defun left-rotate (list)
  "Move the first element to the end of the list."
  (append (rest list) (list (first list))))

(defun handle-face (handle &optional check-only)
  (if-let (watching (member handle *watching-handles* :test #'equal :key #'car))
    (list :foreground  (second (car watching))
          :background *watching-background*
          :weight 'bold)
    (or (gethash handle *handle-faces*)
        (unless check-only
          (prog1
              (setf (gethash handle *handle-faces*) (first *faces*))
            (setf *faces* (left-rotate *faces*)))))))

(defun word-wrap (string &key (width 70) (indent 0))
  (format nil (concatenate 'string
                           "~{~<~%~" (princ-to-string indent)
                           "T~1," (princ-to-string width) ":;~A~> ~}")
          (split-sequence #\space string)))

(defgeneric print-irc-message (type data stream)
  (:method (type data stream))
  (:method ((type (eql :privmsg)) data stream)
    (destructuring-bind (source target message) data
      (declare (ignore target))
      (format t "<~a> ~a~%~%" source
              (word-wrap message :indent (+ (length source) 3)))))
  (:method ((type (eql :action)) data stream)
    (destructuring-bind (source target message) data
      (declare (ignore target))
      (format t "** ~a ~a~%~%" source
              (word-wrap message :indent (+ (length source) 3)))))
  (:method :around ((type (eql :privmsg)) data (stream emacs-output-stream))
    (write-emacs-command stream (call-next-method)))
  (:method ((type (eql :privmsg)) data (stream emacs-output-stream))
    (destructuring-bind (source target message) data
      (declare (ignore target))
      `(insert
        ,(propertize source (handle-face source)) " "
        ,@(let* ((pos (or (position #\: message) (position #\, message)))
                 (color (and pos (handle-face (subseq message 0 pos) t)))
                 (to (and color (subseq message 0 pos)))
                 (message (or (and color (subseq message pos)) message)))
            `(,@(when color
                  (list (propertize to color)))
                ,(word-wrap message :indent (+ 1 (length source)))))
        ,#\newline ,#\newline)))
  (:method :around ((type (eql :action)) data (stream emacs-output-stream))
    (write-emacs-command stream (call-next-method)))
  (:method ((type (eql :action)) data (stream emacs-output-stream))
    (destructuring-bind (source target message) data
      (declare (ignore target))
      `(insert
        "* " ,(propertize source (handle-face source)) " "
        ,(propertize (word-wrap message :indent (+ (length source) 1)) '(:weight 'bold))
        ,#\newline ,#\newline))))

(defun call-sentry (&optional since)
  (call-simple-server since :host *sentry-hostname* :port *sentry-port*))

(defun latest-irc (&optional (since (gethash-database 'last-irc-message-time))
                   (stream *chats-stream*))
  "Print all the unread irc messages."
  (when (typep stream 'emacs-output-stream)
    (write-emacs-command stream '(progn
                                  (setf buffer-read-only nil)
                                  (end-of-buffer)
                                  (push-mark))))
  (iter (for messages on (call-sentry since))
        (for message = (car messages))
        (print-irc-message (first message) (cddr message) stream)
        (when (null (cdr messages))
          (setf (gethash-database 'last-irc-message-time) (second message))))
  (when (typep stream 'emacs-output-stream)
    (write-emacs-command stream '(progn
                                  (font-lock-mode t)
                                  (setf buffer-read-only t)
                                  (goto-char (mark)))))
  (force-output stream))

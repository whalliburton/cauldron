;; simple-server.lisp

(in-package :utilities)

(defclass simple-server ()
  ((host :initform "localhost" :initarg :host)
   (port :initform 5020 :initarg :port)
   (handler :initarg :handler :initform 'simple-server-default-handler)
   accept-thread
   listen-socket))

(defmethod initialize-instance :after ((server simple-server) &key)
  (with-slots (host port listen-socket accept-thread) server
    (setf listen-socket (socket-listen host port :reuseaddress t)
          accept-thread (make-thread (lambda () (simple-server-accept server))
                                     :name "simple-server-accept"))))

(defmethod print-object ((obj simple-server) stream)
  (print-unreadable-object (obj stream :type t)
    (with-slots (host port accept-thread) obj
      (format stream "~A ~A ~A" host port
              (if (thread-alive-p accept-thread) :alive :dead)))))

(defun stop-simple-server (simple-server)
  (with-slots (accept-thread listen-socket) simple-server
    (destroy-thread accept-thread)
    (socket-close listen-socket)))

(defun simple-server-accept (server)
  (with-slots (handler listen-socket) server
    (iter (let* ((*read-eval* nil)
                 (socket (socket-accept listen-socket))
                 (stream (socket-stream socket))
                 (val (read stream)))
            (prin1 (ignore-errors (funcall handler val)) stream)
            (terpri stream)
            (force-output stream)
            (socket-close socket)))))

(defun simple-server-default-handler (val)
  (format t "simple server recieved: ~S~%" val))

(defun call-simple-server (val &key (host "localhost") (port 5020))
  (let* ((socket (socket-connect host port))
         (stream (socket-stream socket)))
    (prin1 val stream)
    (terpri stream)
    (force-output stream)
    (prog1 
        (let ((*read-eval* nil))
          (read stream))
      (socket-close socket))))

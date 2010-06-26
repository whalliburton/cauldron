;; music.lisp

(in-package :music)

(defvar *timidity* nil)
(defvar *messages* (make-mailbox))
(defvar *message-read-thread* nil)

(defun running ()
  (and *timidity* (process-alive-p *timidity*)))

(defun start ()
  (if (running)
    (warn "Timidity is already running.")
    (progn
      (setf *timidity* (run-program "/usr/bin/timidity" '("-ie" "") :input :stream :output :stream :wait nil))
      (make-thread #'message-read-thread :name "message-read-thread"))))

(defun message-read-thread ()
  (loop (send-message *messages* (read (sb-ext:process-output *timidity*)))))

(defun stop ()
  (if (running)
    (progn 
      (destroy-thread *message-read-thread*)
      (process-kill *timidity* sb-unix:sigkill))
    (warn "Timidity is not running.")))

(defun send-command (command)
  (unless (running)
    (error "Timidity is not running"))
  (let ((stream (process-input *timidity*)))
    (format stream "~a~%" command)
    (force-output stream)))

(defparameter *song* "/home/conrad/music/beethoven/Sonate01_Opus2_1.mid")
(defparameter *song2* "/home/conrad/music/beethoven/Sonate02_Opus2_2.mid")

(defun play (filename)
  (send-command "L")
  (send-command (format nil "PLAY ~a" filename)))

(defun messages ()
  (mapcar 'process-message (receive-pending-messages *messages*)))

(defvar *total-time* nil)

(defun process-message (message)
  (destructuring-bind (type . args) message
    (case type
      (timidity-note (apply 'process-note args))
      (timidity-curt (apply 'process-current-time args))
      (timidity-time (list :total-time (setf *total-time* (first args))))
      (timidity-mvol (list :main-volume (first args)))
      (timidity-drums (list :drum-channels (first args)))
      (timidity-reset :reset)
      (t message))))

(defun process-note (ch note stat)
  (list ch
        note
        (aref "cCdDefFgGaAb" (mod note 12))
        (case stat
          (0 :free)
          (1 :on)
          (2 :sustained)
          (3 :off)
          (4 :die))))

(defun process-current-time (secs v)
  (list secs v))

(defun pause-toggle ()
  (send-command " "))

(defun graphics-toggle ()
  (send-command "g"))
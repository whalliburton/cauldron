;; midi.lisp

(in-package :music)

(defparameter *help-text* "Play music.")

(defvar *currently-playing* nil)

(defvar *timidity* nil)
(defvar *messages* (make-mailbox))
(defvar *message-read-thread* nil)

(defun running ()
  (and *timidity* (process-alive-p *timidity*)))

(defun start ()
  (if (running)
    (warn "Timidity is already running.")
    (progn
      (setf *timidity* 
            (run-program "/usr/bin/timidity" '("-ie" "") 
                         :input :stream :output :stream :wait nil)
            *message-read-thread*
            (make-thread #'message-read-thread :name "message-read-thread")))))

(defun start-if-not-running ()
  (unless (running) (start)))

(defun message-read-thread ()
  (loop (send-message *messages* (read (sb-ext:process-output *timidity*)))))

(defun stop-midi ()
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

(defparameter *midi-song* "/lisp/projects/music/songs/Sonate14_Opus27_2_ClairDeLune.mid")

(defun play-midi (&optional (filename *midi-song*))
  "Start playing a midi song with filename FILENAME."
  (start-if-not-running)
  (send-command "L")
  (send-command (format nil "PLAY ~a" filename))
  (setf *currently-playing* (list :midi filename)))

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

(defun quit-timidity ()
  (send-command "Q"))

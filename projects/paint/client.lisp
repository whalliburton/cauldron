;; client.lisp

(in-package :paint-client)

(defvar *paint-hostname* "localhost")
(defvar *paint-port* 5021)

(defun paint (command)
  (call-simple-server command :host *paint-hostname* :port *paint-port*))

(defun start-paint-server ()
  (start-agent 'paint "/lisp/projects/paint/start"))

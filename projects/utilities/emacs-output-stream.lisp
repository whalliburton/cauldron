;; emacs-output-stream.lisp

(in-package :utilities)

;; For EMACS-OUTPUT-STREAM to work you need to enable in emacs:
;; (setq slime-enable-evaluate-in-emacs t)

(defclass emacs-output-stream (fundamental-character-output-stream)
  ((name :initform "*lisp-output-stream*" :initarg :name)
   (queue :initform nil :accessor queue)
   (character-queue :initform nil :accessor character-queue)))

(defmethod print-object ((obj emacs-output-stream) stream)
  (print-unreadable-object (obj stream :type t)
    (write-string (slot-value obj 'name) stream)))

(defmethod stream-write-char ((stream emacs-output-stream) char)
  (push char (character-queue stream)))

(defun push-characters-to-queue (emacs-stream)
  (let ((queue (character-queue emacs-stream)))
    (when queue
      (push `(insert ,(coerce (nreverse queue) 'string)) (queue emacs-stream))
      (setf (character-queue emacs-stream) nil))))

(defun write-emacs-command (emacs-stream command)
  (push-characters-to-queue emacs-stream)
  (push command (queue emacs-stream)))

(defmethod stream-force-output ((stream emacs-output-stream))
  (push-characters-to-queue stream)
  (with-slots (queue name) stream
    (when queue
      (swank::eval-in-emacs
       `(progn
          (with-current-buffer (get-buffer-create ,name)
            (end-of-buffer)
            ,@(nreverse queue))
          (unless (string= (buffer-name (current-buffer)) ,name)
            (switch-to-buffer-other-window ,name))
          nil))
      (setf queue nil))))

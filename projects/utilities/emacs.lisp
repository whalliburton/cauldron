;; emacs.lisp

(in-package :utilities)

(defvar *ssh-index* 0)

(defun ssh-in-emacs (arguments &key agent-forwarding)
  (swank::eval-in-emacs 
   `(progn
      (let ((buffer (shell ,(format nil "* ssh ~A ~A *" arguments (incf *ssh-index*)))))
        (sleep-for 1)
        (insert ,(format nil "/usr/bin/ssh -o StrictHostKeyChecking=no ~:[~;-A ~]~A"
                         agent-forwarding arguments))
        (save-current-buffer
          (set-buffer buffer)
          (comint-send-input)
          (setf comint-process-echoes t)))
      nil)))


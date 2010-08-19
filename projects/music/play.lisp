;; play.lisp

(in-package :music)

(defparameter *help-text* "Play music.")

(defvar *currently-playing* nil)

(defun play (&optional what)
  "Play an audio file."
  (if what
    (if (not (probe-file what))
      (format t "No file named ~s found." what)
      (let ((mime (magic-mime what)))
        (cond 
          ((string= mime "audio/midi") (play-midi what))
          ((string= mime "application/ogg") (play-ogg what))
          (t (format t "Unsupported file format ~s." mime)))))
    (play-midi)))

(defun stop ()
  "Stop anything playing."
  (if (null *currently-playing*)
    (format t "Nothing is currently playing.~%")
    (progn
      (ecase (car *currently-playing*)
        (:midi (stop-midi))
        (:ogg (stop-ogg)))
      (setf *currently-playing* nil))))

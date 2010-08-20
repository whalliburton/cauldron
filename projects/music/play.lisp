;; play.lisp

(in-package :music)

(defparameter *help-text* "Play music.")

(defgeneric %play (what)
  (:method (what)
   (if what
     (if (not (probe-file what))
       (format t "No file named ~s found." what)
       (let ((mime (magic-mime what)))
         (cond 
           ((string= mime "audio/midi") (play-midi what))
           ((string= mime "audio/x-wav") (play-wav (read-wav what)))
           ((string= mime "application/ogg") (play-ogg what))
           (t (format t "Unplayable file format ~s.~%" mime)))))
     (play-midi)))
  (:method ((what cached-http-request))
    (%play (namestring (blob-pathname what)))))

(defun play (&optional what)
  "Play music or sounds."
  (%play what))

(defun stop ()
  "Stop anything playing."
  (if (null *currently-playing*)
    (format t "Nothing is currently playing.~%")
    (progn
      (ecase (car *currently-playing*)
        (:midi (stop-midi))
        (:ogg (stop-ogg)))
      (setf *currently-playing* nil))))

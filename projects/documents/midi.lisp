;; midi.lisp

(in-package :documents)

(defclass midi-file (base-document)
  ()
  (:metaclass persistent-class))

(defmethod read-document ((document midi-file))
  (setf *inhibit-read-message* t)
  (describe-midi (namestring (blob-pathname document))))

(defmethod import-document (name (type (eql :|audio/midi|)))
   (import-simple-document name 'midi-file))

(defun midi-note-number-to-note (number)
  (multiple-value-bind (octave note) (floor number 12)
    (format nil "~A~A" 
            (aref #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B") note) 
            (1- octave))))

(defun describe-midi (filename)
  (iter (for track in (midi:midifile-tracks (midi:read-midi-file filename)))
        (for x upfrom 1)
        (format t "~%Track ~A~%" x)
        (iter (for message in track)
              (pretty-print-midi-message message))))

(defun midi-message-name (message)
  (let ((name (symbol-name (type-of message))))
    (substitute #\space #\- (string-downcase (subseq name 0 (- (length name) 8))))))

(defparameter *show-midi-status* t)

(defgeneric pretty-print-midi-message (message)
  (:method :before (message) 
    (format t "  ~@[~2X  ~]~30A" (when *show-midi-status* (midi::message-status message))
            (midi-message-name message)))
  (:method (message))
  (:method :after (message) (format t "~%"))
  (:method ((message midi::text-message))
    (format t "~A" (slot-value message 'midi::text)))
  (:method ((message midi::note-on-message))
    (format t "~A" (midi-note-number-to-note (midi::message-key message))))
  (:method ((message midi::note-off-message))
    (format t "~A" (midi-note-number-to-note (midi::message-key message))))
  (:method ((message midi::pitch-bend-message))
    (format t "~A" (midi::message-value message))))


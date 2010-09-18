;; octaves.lisp

(in-package :music)

(defun 12TET-frequency (desired-pitch-number 
                       &key (reference 440.0) (reference-pitch-number 49))
  (* reference (expt (expt 2 (/ 1 12)) (- desired-pitch-number reference-pitch-number))))

(defun key-number-to-note (key-number)
  (multiple-value-bind (octave note) (floor (+ key-number 8) 12)
    (format nil "~A~A" 
            (aref #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B") note) 
            octave)))

(defun play-key (key)
  (play-tone :frequency (12TET-frequency key) :wait t))

(defun play-octave (&optional (start-key 40))
  "Play an octave of equal temperament tones."
  (iter (for key from start-key to (+ start-key 12))
        (format t "~4A ~,4F~%" (key-number-to-note key) (12TET-frequency key))
        (play-key key)))

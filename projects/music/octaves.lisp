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

(defun note-to-pitch-number (note)
  (multiple-value-bind (all note sharp octave)
      (#~c/([ABCDEFG])(#?)([0123456789]?)/ (ensure-string note))
    (declare (ignore all))
    (let ((octave (or (and (plusp (length octave)) (parse-integer octave)) 4))
          (sharp (plusp (length sharp)))
          (note (case (char note 0)
                  (#\C 0) (#\D 2) (#\E 4) (#\F 5) (#\G 7) (#\A 9) (#\B 11))))
      (+ (* octave 12) note (if sharp 1 0) -8))))

(defun play-note (note)
  (play-key (note-to-pitch-number note)))

(defun play-notes (notes)
  (mapc #'play-note notes))

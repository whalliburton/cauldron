;; noise.lisp

(in-package :music)

(defun make-frequency-streamer (&optional (frequency 440) (duration 44100) destroy)
  (let ((phase 0.0)
        (total-time 0))
    (lambda (streamer mixer buffer offset length time)
      (declare (ignore time))
      (iter (for index upfrom offset)
            (repeat length)
            (with dp = (* 2.0 pi frequency 1/44100))
            (mixalot:stereo-incf (aref buffer index) (mixalot:mono->stereo
                                                      (round (* 20000 (sin phase)))))
            (incf phase dp))
      (incf total-time length)
      (when (> total-time duration)
        (mixalot:mixer-remove-streamer mixer streamer)
        (when destroy (destroy-mixer mixer))))))

(defun play-tone (&key (frequency 440) (duration 44100) wait)
  (let ((mixer (create-mixer)))
    (mixer-add-streamer mixer (make-frequency-streamer frequency duration t))
    (when wait (sleep (/ duration 44100))))
  (values))

;;; wav.lisp

;;; Adapted from CL-Wav-Synth by Philippe Brochard (hocwp@free.fr)
;;; Merci beaucoup, monsieur Philippe Brochard.

(in-package :wav)

;;; converters

(defun time-to-sample (n-samples-per-sec time)
  "Convert time in sample according to frequence n-samples-per-sec."
  (truncate (* n-samples-per-sec time)))

(defun sample-to-time (n-samples-per-sec n-sample)
  "Convert sample in time according to frequence n-samples-per-sec."
  (/ n-sample n-samples-per-sec))

;;; main class

;; samples-per-sec      Hz
;; channels             chan
;; bits-per-sample      bits/sample
;; block-align          bytes
;; avg-bytes-per-sec    bytes/s
;; total-byte           bytes
;; last-sample          (data sample length)
;; max-index            (one channel length)
;; time                 s

(defclass wav ()
  ((channels :initarg :channels :accessor channels)
   (compression :initarg :compression :accessor compression)
   (samples-per-sec :initarg :samples-per-sec :accessor samples-per-sec)
   (bits-per-sample :initarg :bits-per-sample :accessor bits-per-sample)
   (time :initarg :time :initform nil :accessor sample-time)
   (avg-bytes-per-sec :initform nil :accessor avg-bytes-per-sec)
   (block-align :initform nil :accessor block-align)
   (total-byte :initform nil :accessor sample-total-byte)
   (last-sample :initform nil :accessor sample-last-sample)
   (max-index :initform nil :accessor sample-max-index)
   (max-ampl :initform nil :accessor sample-max-ampl)
   (data :initarg :data :initform nil :accessor data)))

(defmethod print-object ((wav wav) stream)
  (with-slots (channels time) wav
    (print-unreadable-object (wav stream :type t)
      (format stream "~A ~A" channels time))))

;;; Low level functions helper

(defun read-id (stream size)
  (let ((answer (loop for i from 1 to size
                      collect (read-byte stream))))
    (map 'string #'code-char answer)))

(defun read-16 (stream)
  (let ((answer (read-byte stream)))
    (when answer
      (setf (ldb (byte 8 8) answer) (read-byte stream)))
    answer))

(defun read-32 (stream)
  (let ((answer (read-byte stream)))
    (when answer
      (setf (ldb (byte 8 8) answer) (read-byte stream))
      (setf (ldb (byte 8 16) answer) (read-byte stream))
      (setf (ldb (byte 8 24) answer) (read-byte stream)))
    answer))

(defun t->s (sample period)
  "Convert time period to samples."
  (with-slots (samples-per-sec max-index) sample
    (let ((s (time-to-sample samples-per-sec period)))
      (if (and max-index (>= s max-index))
          (1- max-index)
          s))))

(defun s->t (sample index)
  "Convert samples to time period."
  (with-slots (samples-per-sec) sample
    (sample-to-time samples-per-sec index)))

(defun f->s (sample freq)
  "Convert frequence to samples."
  (truncate (/ (samples-per-sec sample) freq)))

(defun find-smin-smax (sample start end)
  "Find min and max samples index from min and max time in a sample."
  (with-slots (last-sample channels) sample
    (values (if start
                (* (t->s sample start) channels)
                0)
            (if end
                (* (1+ (t->s sample end)) channels)
                last-sample))))

(defun skip-header (sample)
  (/ 352 (bits-per-sample sample)))

(defun read-header (filename sample)
  "Read wav header info."
  (labels ((expected (read-str orig-str)
             (assert (string= read-str orig-str) ()
                     "error reading header: ~S is not a wav file. Expected ~A Got ~A"
                     filename orig-str read-str)))
    (with-slots (samples-per-sec
                 channels compression bits-per-sample
                 block-align avg-bytes-per-sec total-byte) sample
      (with-open-file (stream filename :direction :input
                              :element-type '(unsigned-byte 8))
        (expected (read-id stream 4) "RIFF")
        (read-32 stream)
        (expected (read-id stream  4) "WAVE")
        (loop
         (let* ((next-header (read-id stream 4))
                (bytes (read-32 stream)))
           (cond ((string= next-header "fmt ")
                  (setf compression (read-16 stream))
                  (setf channels (read-16 stream))
                  (setf samples-per-sec (read-32 stream))
                  (setf avg-bytes-per-sec (read-32 stream))
                  (setf block-align (read-16 stream))
                  (setf bits-per-sample (read-16 stream))
                  ;; possible extra format bytes
                  (dotimes (i (- bytes 16)) (read-byte stream)))
                 ((string= next-header "data")
                  (setf total-byte bytes)
                  (return))
                 (t
                  ;; There're a lot of headers that we don't
                  ;; care. For instance, bext minf elmo, etc
                  (dotimes (i bytes) (read-byte stream)))))))))
  sample)

(defun set-sample-info (sample)
  (with-slots (samples-per-sec
               channels bits-per-sample
               block-align avg-bytes-per-sec max-ampl) sample
    (setf block-align (* channels (/ bits-per-sample 8)))
    (setf avg-bytes-per-sec (* samples-per-sec block-align))
    (setf max-ampl (1- (expt 2 (1- bits-per-sample))))))

(defun set-total-byte-from-time (sample)
  (with-slots (samples-per-sec
               total-byte block-align time) sample
    (when (numberp time)
      (setf total-byte
            (* block-align (time-to-sample samples-per-sec time))))))

(defun set-last-sample (sample)
  (with-slots (bits-per-sample
               samples-per-sec
               last-sample total-byte max-index channels time) sample
    (setf last-sample (if (numberp total-byte)
                          (/ total-byte (/ bits-per-sample 8))
                          0)
          max-index (/ last-sample channels)
          time (float (sample-to-time samples-per-sec max-index)))))

;;; Data functions

(defun make-data (size bits-per-sample)
  (make-array size
              :element-type `(signed-byte ,bits-per-sample)
              :initial-element 0))

;;; spectre functions

(defclass spectre (data)
  ((im :initarg :im :initform 0 :accessor spectre-im)
   (time :initarg :time :initform 0 :accessor spectre-time)
   (samples-per-sec :initarg :samples-per-sec)
   (bits-per-sample :initarg :bits-per-sample)))

(defun  freq->index (spectre freq)
  (floor (* freq (spectre-time spectre))))

;;; sample functions

(defmacro get-ampl (sample chan index)
  `(aref (data ,sample)
    (+ (* ,index (channels ,sample)) ,chan)))

(defun set-ampl (sample fun chan index ampl)
  (with-slots (max-ampl) sample
    (let ((val (truncate (funcall fun
                                  (get-ampl sample chan index)
                                  ampl))))
      (setf (get-ampl sample chan index)
            (cond ((>= val max-ampl) max-ampl)
                  ((<= val (- max-ampl)) (- max-ampl))
                  (t val))))))

(defun set-total-byte-from-data (sample)
  (with-slots (bits-per-sample data total-byte) sample
    (setf total-byte
          (* (/ bits-per-sample 8) (length data)))))

(defun set-data (sample)
  (with-slots (total-byte
               bits-per-sample data last-sample) sample
    (when (numberp total-byte)
      (setf data (make-data last-sample bits-per-sample)))))

(defmethod initialize-instance :after ((wav wav) &key)
  (set-sample-info wav)
  (set-total-byte-from-time wav)
  (set-last-sample wav)
  (set-data wav))

(defun read-wav (filename &key start end)
  (let ((sample (make-instance 'wav :channels 1
                               :samples-per-sec 22050
                               :bits-per-sample 16)))
    (with-slots (bits-per-sample data) sample
      (read-header filename sample)
      (set-sample-info sample)
      (set-last-sample sample)
      (multiple-value-bind (smin smax)
          (find-smin-smax sample start end)
        (setf data (make-data (- smax smin) bits-per-sample))
        (with-open-file (stream filename :direction :input
                                :element-type
                                `(signed-byte ,bits-per-sample))
          (file-position stream (+ (skip-header sample) smin))
          (read-sequence data stream)))
      (set-total-byte-from-data sample)
      (set-last-sample sample)
      sample)))


;;; === unused ===

(defun find-s (sample time)
  "Find samples index from time in a sample."
  (with-slots (channels) sample
    (if time
        (* (t->s sample time) channels)
        0)))

(defun apply-on-sample (sample fun &key args start end)
  (with-slots (samples-per-sec bits-per-sample channels data) sample
    (let ((new-sample (make-instance 'wav
                                     :channels channels
                                     :samples-per-sec samples-per-sec
                                     :bits-per-sample bits-per-sample))
          val)
      (set-sample-info new-sample)
      (multiple-value-bind (smin smax)
          (find-smin-smax sample start end)
        (with-slots ((new-data data) max-ampl) new-sample
          (setf new-data (copy-seq data))
          (loop for i from smin below smax do
                (setf val (truncate (apply fun i
                                           (aref data i)
                                           args)))
                (setf (aref new-data i)
                      (cond ((>= val max-ampl) max-ampl)
                            ((<= val (- max-ampl)) (- max-ampl))
                            (t val))))))
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      new-sample)))

(defun cut-sample (sample start end)
  "Return a new sample without the cutted part."
  (with-slots (samples-per-sec
               bits-per-sample channels data) sample
    (let ((new-sample (make-instance 'wav
                                     :channels channels
                                     :samples-per-sec samples-per-sec
                                     :bits-per-sample bits-per-sample)))
      (set-sample-info new-sample)
      (multiple-value-bind (smin smax)
          (find-smin-smax sample start end)
        (setf (data new-sample) (concatenate 'vector
                                             (subseq data 0 smin)
                                             (subseq data smax))))
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      new-sample)))

(defun header-equal (sample1 sample2)
  "Compare only significant slot from two headers."
  (and (equal (channels sample1)
              (channels sample2))
       (equal (bits-per-sample sample1)
              (bits-per-sample sample2))
       (equal (samples-per-sec sample1)
              (samples-per-sec sample2))))

(defun insert-sample (sample sample2 start)
  "Return a new sample with sample2 inserted at start seconds"
  (with-slots (samples-per-sec
               bits-per-sample channels data max-index) sample
    (assert (header-equal sample sample2)
            ()
            "error samples must have the same format")
    (let ((new-sample (make-instance 'wav
                                     :channels channels
                                     :samples-per-sec samples-per-sec
                                     :bits-per-sample bits-per-sample))
          (ind (time-to-sample samples-per-sec start)))
      (set-sample-info new-sample)
      (setf (data new-sample)
            (if (< ind max-index)
                (concatenate 'vector
                             (subseq data 0 ind)
                             (data sample2)
                             (subseq data ind))
                (concatenate 'vector
                             data
                             (make-array (- ind max-index) :initial-element 0
                                         :element-type `(signed-byte ,bits-per-sample))
                             (data sample2))))
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      new-sample)))

(defun calc-new-len (len1 len2 start)
  "Return the new length of a sample with 1 channel
        <--- len 1 --->
             <----- len 2 ----->
        ^    |_start           ^
        |----- new len --------|"
  (let ((rest (- (+ len2 start) len1)))
    (+ len1 (if (> rest 0) rest 0))))


(defun mix-sample (sample1 sample2 fun &key args (start 0))
  (with-slots ((channels-1 channels)
               (bits-per-sample-1 bits-per-sample)
               (samples-per-sec-1 samples-per-sec)
               (data-1 data)) sample1
    (with-slots ((channels-2 channels)
                 (bits-per-sample-2 bits-per-sample)
                 (samples-per-sec-2 samples-per-sec)
                 (data-2 data)) sample2
      (assert (header-equal sample1 sample2)
              () "error sample and new channel must have the same format")
      (assert (>= start 0) ()
              "error start must be a null or positive time in seconds")
      (let* ((len1 (length data-1))
             (len2 (length data-2))
             (s-start (* (time-to-sample samples-per-sec-2 start) channels-2))
             (new-len (* (calc-new-len (/ len1 channels-1)
                                       (/ len2 channels-2)
                                       (/ s-start channels-2))
                         channels-1))
             (new-sample (make-instance 'wav :channels channels-1
                                        :bits-per-sample bits-per-sample-1
                                        :samples-per-sec samples-per-sec-1
                                        :data (make-data new-len
                                                         bits-per-sample-1)))
             val)
        (set-sample-info new-sample)
        (set-total-byte-from-data new-sample)
        (set-last-sample new-sample)
        (with-slots (data max-ampl) new-sample
          (dotimes (i new-len)
            (setf val (truncate (apply fun i
                                       (if (< i len1) (aref data-1 i) 0)
                                       (if (< s-start i (+ s-start len2))
                                           (aref data-2 (- i s-start))
                                           0)
                                       args)))
            (setf (aref data i)
                  (cond ((>= val max-ampl) max-ampl)
                        ((<= val (- max-ampl)) (- max-ampl))
                        (t val)))))
        new-sample))))

(defun extract-channel (sample chan)
  (with-slots (channels
               bits-per-sample samples-per-sec data) sample
    (assert (<= 0 chan (1- channels)) ()
            "error new channel can't be ~A (chan=[0..~A])"
            chan (1- channels))
    (let* ((len (/ (length data) channels))
           (new-sample (make-instance 'wav :channels 1
                                      :bits-per-sample bits-per-sample
                                      :samples-per-sec samples-per-sec
                                      :data (make-data len
                                                       bits-per-sample))))
      (set-sample-info new-sample)
      (set-total-byte-from-data new-sample)
      (set-last-sample new-sample)
      (with-slots ((new-data data)) new-sample
        (dotimes (i len)
          (setf (aref new-data i)
                (aref data (+ (* i channels) chan)))))
      new-sample)))


;;; data:   |1 2|1 2|1 2|1 2|1 2|   2 channels
;;; New-chan-data: |3|3|3|
;;; => new-data:   |1 2 3|1 2 3|1 2 3|1 2 0|1 2 0| with start=0
;;; => new-data:   |1 2 0|1 2 3|1 2 3|1 2 3|1 2 0| with start=1
;;; => new-data:   |1 2 0|1 2 0|1 2 0|1 2 3|1 2 3|1 2 3| with start=3
(defun add-channel (sample chan new-chan &key (start 0))
  (with-slots ((channels-1 channels)
               (bits-per-sample-1 bits-per-sample)
               (samples-per-sec-1 samples-per-sec)
               (data-1 data)) sample
    (with-slots ((channels-2 channels)
                 (bits-per-sample-2 bits-per-sample)
                 (samples-per-sec-2 samples-per-sec)
                 (data-2 data)) new-chan
      (assert (= channels-2 1) ()
              "error new channel must be a 1 channel sample")
      (assert (<= 0 chan channels-1) ()
              "error new channel can't be put in channel ~A (chan=[0..~A])"
              chan channels-1)
      (assert (and (= bits-per-sample-1 bits-per-sample-2)
                   (= samples-per-sec-1 samples-per-sec-2))
              ()
              "error sample and new channel must have the same format")
      (assert (>= start 0) ()
              "error start must be a null or positive time in seconds")
      (let* ((len1 (/ (length data-1) channels-1))
             (len2 (length data-2))
             (s-start (time-to-sample samples-per-sec-2 start))
             (new-chan (1+ channels-1))
             (new-sample (make-instance 'wav :channels new-chan
                                        :bits-per-sample bits-per-sample-1
                                        :samples-per-sec samples-per-sec-1
                                        :data
                                        (make-data (* (calc-new-len
                                                       len1 len2 s-start)
                                                      new-chan)
                                                   bits-per-sample-1))))
        (set-sample-info new-sample)
        (set-total-byte-from-data new-sample)
        (set-last-sample new-sample)
        (with-slots (data) new-sample
          (dotimes (i len1)
            (dotimes (j channels-1)
              (setf (aref data (+ (* i new-chan)
                                  (if (>= j chan) (1+ j) j)))
                    (aref data-1 (+ (* i channels-1) j)))))
          (dotimes (i len2)
            (setf (aref data (+ (* (+ i s-start) new-chan) chan))
                  (aref data-2 i)))
          new-sample)))))

;;; Time to Freq functions

(defun fft (rl im dir)
  (let* ((len (length rl))
         (n (if (oddp len) (1- len) len))
         (n/2 (/ n 2))
         (imh (truncate (/ (log (1+ n)) (log 2.0)))))
    ;; bits inversion
    (loop for i below n
          with j = 0
          with m do
          (when (> j i)
            (rotatef (aref rl j) (aref rl i))
            (rotatef (aref im j) (aref im i)))
          (setf m n/2)
          (loop while (and (>= m 2) (>= j m)) do
                (setf j (- j m)
                      m (truncate (/ m 2))))
          (setf j (+ j m)))
    ;; FFT calculation
    (loop for lg below imh
          with m = 2
          with ldm = 1
          with mh = n/2
          with angle = (* pi dir)
          with i    with j   with u
          with ur   with ui
          with vr   with vi
          with c    with s do
          (setf c (cos angle)
                s (sin angle)
                ur 1.0
                ui 0.0)
          (loop for i2 below ldm do
                (setf i i2
                      j (+ i2 ldm))
                (loop for j2 below mh do
                      (setf vr (- (* ur (aref rl j)) (* ui (aref im j)))
                            vi (+ (* ur (aref im j)) (* ui (aref rl j)))
                            (aref rl j) (- (aref rl i) vr)
                            (aref im j) (- (aref im i) vi))
                      (incf (aref rl i) vr)
                      (incf (aref im i) vi)
                      (incf i m)
                      (incf j m))
                (setf u ur
                      ur (- (* ur c) (* ui s))
                      ui (+ (* ui c) (* u s))))
          (setf mh (truncate (/ mh 2))
                ldm m
                angle (* angle 0.5)
                m (* m 2)))
    (when (= dir 1)
      (dotimes (i len)
        (setf (aref rl i) (/ (aref rl i) n))
        (setf (aref im i) (/ (aref im i) n))))
    (values rl im)))

(defmethod time<->freq ((wav wav))
  (with-slots (samples-per-sec
               data bits-per-sample channels time) wav
    (assert (= channels 1) ()
            "error sample must have exactly one channel and not ~A"
            channels)
    (let ((spectre (make-instance 'spectre :time time
                                  :samples-per-sec samples-per-sec
                                  :bits-per-sample bits-per-sample
                                  :data (make-array (length data)
                                                    :initial-contents
                                                    (copy-seq data))
                                  :im (make-array (length data)
                                                  :initial-element 0))))
      (fft (data spectre) (spectre-im spectre) 1)
      spectre)))

(defmethod time<->freq ((spectre spectre))
  (with-slots (samples-per-sec
               bits-per-sample data im time) spectre
    (print 'debut)
    (let* ((len (length data))
           (sample (make-instance 'wav :channels 1
                                  :samples-per-sec samples-per-sec
                                  :bits-per-sample bits-per-sample
                                  :time time
                                  :data (make-data (length data)
                                                   bits-per-sample)))
           (rl (copy-seq data))
           (im (copy-seq im)))
      (set-sample-info sample)
      (format t "Max spectre=~A~%"
              (loop for i across rl maximize i))
      (fft rl im -1)
      (print 'ici)
      (format t "Max spectre=~A~%"
              (loop for i across rl maximize i))
      (with-slots (max-ampl) sample
        (let (val)
          (dotimes (i (length rl))
            (setf val (truncate (* (aref rl i) len)))
            (setf (aref (data sample) i)
                  (cond ((>= val max-ampl) max-ampl)
                        ((<= val (- max-ampl)) (- max-ampl))
                        (t val))))))
      (format t "Max spectre=~A~%"
              (loop for i across (data sample) maximize i))
      (set-total-byte-from-data sample)
      (set-last-sample sample)
      sample)))


;;; Generators

(defun sample-make-square (sample fun chan start end freq minampl maxampl)
  "Add a square on sample."
  (let* ((smin (t->s sample start))
         (smax (min (t->s sample end)
                    (- (sample-max-index sample) 2)))
         (sfreq (f->s sample freq))
         (sfreq/2 (/ sfreq 2)))
    (loop for i from smin to smax do
          (set-ampl sample fun chan i
                    (if (> sfreq/2 (mod (- i smin) sfreq))
                        maxampl minampl)))))

(defmethod sample-make-line (sample fun chan start end freq minampl maxampl)
  "Add a line on sample."
  (let* ((smin (t->s sample start))
         (smax (min (t->s sample end)
                    (- (sample-max-index sample) 2)))
         (sfreq (f->s sample freq))
         (dy (/ (- maxampl minampl) sfreq)))
    (loop for i from smin to smax do
          (set-ampl sample fun chan i
                    (+ minampl (* (mod (- i smin) sfreq) dy))))))


(defmethod sample-make-line* (sample fun chan start end freq ampls)
  "Add lines on sample. Ampls is a list like '((val1 ampl1) (val2 ampl2)...)
   where 0 <= val <= 1."
  (let* ((smin (t->s sample start))
         (smax (t->s sample end))
         (sfreq (f->s sample freq))
         (points (mapcar #'(lambda (x)
                             (list (* (first x) sfreq) (second x)))
                         ampls)))
    (labels ((line (mins maxs mina maxa)
               (let ((dy (/ (- maxa mina) (- maxs mins))))
                 (loop for i from smin to smax do
                       (when (<= mins (mod (- i smin) sfreq) maxs)
                         (set-ampl sample fun chan i
                                   (+ mina (* (mod (- i smin mins) sfreq)
                                              dy))))))))
      (loop for last = (first points) then p
            for p in (cdr points) do
            (line (first last) (first p) (second last) (second p))))))


(defvar *spi* (float pi 1f0))

(defun sample-make-sin (sample fun chan start end freq
                        ampl sin-fun &optional (phase 0))
  "Add a sinus to sample."
  (let* ((smin (t->s sample start))
         (smax (min (t->s sample end)
                    (- (sample-max-index sample) 2)))
         (sfreq (f->s sample freq)))
    (loop for i from smin to smax do
          (set-ampl sample fun chan i
                    (* ampl
                       (funcall sin-fun
                                (+ (* 2 *spi* (/ (- i smin) sfreq)) phase)))))))

(defun random-mm (min max)
  "Return a random number between min and max"
  (+ (random (- max min)) min))

(defun sample-make-noise (sample fun chan start end freq minampl maxampl)
  "Add noise to sample."
  (let* ((smin (t->s sample start))
         (smax (min (t->s sample end)
                    (- (sample-max-index sample) 2)))
         (sfreq (f->s sample freq)))
    (loop for i from smin to smax
          with val = 0 do
          (when (zerop (mod (- i smin) sfreq))
            (setf val (random-mm minampl maxampl)))
          (set-ampl sample fun chan i val))))

(defun build-from-freq (freqs &key (samples-per-sec 22050)
                        (bits-per-sample 16) (time 1))
  "Build a new sample based on frequences and amplitudes
   freqs is a list like this '((freq1 ampl1 phase1) (freq2 ampl2 phase2) ...)"
  (labels ((set-fun (data new)
             (declare (ignore data))
             new))
    (let ((sample (make-instance 'wav :channels 1
                                 :samples-per-sec samples-per-sec
                                 :bits-per-sample bits-per-sample
                                 :time time)))
      (with-slots (max-ampl) sample
        (sample-make-sin sample #'set-fun 0 0 time (first (first freqs))
                         (* max-ampl (second (first freqs))) #'sin)
        (dolist (freq (rest freqs))
          (when (plusp (second freq))
            (sample-make-sin sample #'+ 0 0 time (first freq)
                             (* max-ampl (second freq)) #'sin
                             (third freq)))))
      sample)))

(defmacro with-view ((stream min max) &body body)
  (let ((dataname (gensym))
        (filename (gensym)))
    `(let ((,dataname (format nil "data-~A.log" (gensym)))
           (,filename (format nil "gnuplot-~A.gnuplot" (gensym))))
      (with-open-file (,stream ,filename :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (format ,stream "set mouse~%")
        (format ,stream "plot [~A:~A] ~S with lines~%" ,min ,max ,dataname)
        (format ,stream "pause mouse~%"))
      (with-open-file (,stream ,dataname :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
        (progn
          ,@body))
       ;;      (ushell "gnuplot" ,filename)
      (delete-file ,dataname)
      (delete-file ,filename))))

(defmethod sample-view ((spectre spectre) &key (min 0) (max 1000) (fun #'abs))
  (with-view (stream min max)
    (loop for i from (freq->index spectre min)
          to (freq->index spectre max) do
          (format stream "~A  ~A~%"
                  (/ i (spectre-time spectre))
                  (funcall fun (complex (aref (data spectre) i)
                                        (aref (spectre-im spectre) i)))))))

(defmethod sample-view ((wav wav) &key (min 0) (max 1000) (fun 0))
  (with-view (stream min max)
    (with-slots (max-ampl) wav
      (loop for i from (t->s wav min) to (t->s wav max) do
            (format stream "~A  ~A~%"
                    (float (s->t wav i))
                    (get-ampl wav fun i))))))

(defun play-wav (wav)
  (if (not (eql (bits-per-sample wav) 16))
    (warn "Only 16-bit WAVs are supported.")
    (let* ((rate (samples-per-sec wav))
           (mixer (create-mixer :rate rate)))
      (let ((streamer
             (ecase (channels wav)
               (1 (make-fast-vector-streamer-mono (data wav)))
               (2 (make-fast-vector-streamer-interleaved-stereo (data wav))))))
        (mixer-add-streamer mixer streamer)
        (setf music:*currently-playing* (list :wav wav mixer))))))
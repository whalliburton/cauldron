;; alsa-mixer.lisp

(in-package :music)

(load-foreign-library "/usr/lib/libasound.so")

(defcfun snd-mixer-open :int (mixerp :pointer) (mode :int))
(defcfun snd-mixer-close :int (mixerp :pointer))

(defun open-mixer ()
  (with-foreign-object (ptr :pointer)
    (unless (zerop (snd-mixer-open ptr 0))
      (error "Error opening mixer."))
    (mem-ref ptr :pointer)))

(defun close-mixer (mixer)
  (snd-mixer-close mixer))

(defmacro with-mixer ((var) &rest body)
  `(let ((,var (open-mixer)))
     (unwind-protect
          (progn ,@body)
       (close-mixer ,var))))

(defcfun snd-mixer-attach :int (mixer :pointer) (name :string))
(defcfun snd-mixer-load :int (mixer :pointer))

(defcfun snd-mixer-selem-register :int (mixer :pointer) (options :pointer)
         (classp :pointer))

(defcfun snd-mixer-first-elem :pointer (mixer :pointer))
(defcfun snd-mixer-elem-next :pointer (elem :pointer))
(defcfun snd-mixer-selem-get-name :string (elem :pointer))
(defcfun snd-mixer-selem-has-playback-volume :int (elem :pointer))
(defcfun snd-mixer-selem-has-capture-volume :int (elem :pointer))
(defcfun snd-mixer-selem-has-common-volume :int (elem :pointer))
(defcfun snd-mixer-selem-is-playback-mono :int (elem :pointer))
(defcfun snd-mixer-selem-is-capture-mono :int (elem :pointer))
(defcfun snd-mixer-selem-has-playback-channel :int (elem :pointer) (channel :int))
(defcfun snd-mixer-selem-has-capture-channel :int (elem :pointer) (channel :int))
(defcfun snd-mixer-selem-channel-name :string (channel :int))
(defcfun snd-mixer-selem-get-playback-volume-range :int (elem :pointer)
         (pmin :pointer) (pmax :pointer))
(defcfun snd-mixer-selem-get-capture-volume-range :int (elem :pointer)
         (pmin :pointer) (pmax :pointer))
(defcfun snd-mixer-selem-get-playback-volume :int (elem :pointer)
         (channel :int) (value :pointer))
(defcfun snd-mixer-selem-get-capture-volume :int (elem :pointer)
         (channel :int) (value :pointer))
(defcfun snd-mixer-selem-get-playback-switch :int (elem :pointer)
         (channel :int) (value :pointer))
(defcfun snd-mixer-selem-get-capture-switch :int (elem :pointer)
         (channel :int) (value :pointer))

(defparameter *snd-mixer-schn-last* 31)

(defun mixer-selem-get-playback-volume-range (elem)
  (with-foreign-objects ((min :long) (max :long))
    (snd-mixer-selem-get-playback-volume-range elem min max)
    (list (mem-ref min :long) (mem-ref max :long))))

(defun mixer-selem-get-capture-volume-range (elem)
  (with-foreign-objects ((min :long) (max :long))
    (snd-mixer-selem-get-capture-volume-range elem min max)
    (list (mem-ref min :long) (mem-ref max :long))))

(defun mixer-selem-get-playback-volume (elem channel)
  (with-foreign-object (value :long)
    (snd-mixer-selem-get-playback-volume elem channel value)
    (mem-ref value :long)))

(defun mixer-selem-get-capture-volume (elem channel)
  (with-foreign-object (value :long)
    (snd-mixer-selem-get-capture-volume elem channel value)
    (mem-ref value :long)))

(defun mixer-selem-get-playback-switch (elem channel)
  (with-foreign-object (value :int)
    (snd-mixer-selem-get-playback-switch elem channel value)
    (plusp (mem-ref value :int))))

(defun mixer-selem-get-capture-switch (elem channel)
  (with-foreign-object (value :long)
    (snd-mixer-selem-get-capture-switch elem channel value)
    (plusp (mem-ref value :int))))

(defun decode-mixer-elem (elem)
  (list (snd-mixer-selem-get-name elem)
        (when (snd-mixer-selem-has-playback-volume elem)
          (let ((range (mixer-selem-get-playback-volume-range elem)))
            (list
             range
             (cond
               ((plusp (snd-mixer-selem-is-playback-mono elem)) (list (list nil "Mono")))
               (t (iter (for channel from 0 to *snd-mixer-schn-last*)
                        (when (plusp (snd-mixer-selem-has-playback-channel elem channel))
                          (let ((volume (mixer-selem-get-playback-volume elem channel)))
                            (collect (list channel
                                           (snd-mixer-selem-channel-name channel)
                                           volume
                                           (format nil "~d%" (round (* 100 (/ volume (second range)))))
                                           (if (mixer-selem-get-playback-switch elem channel)
                                             "on" "off")
                                           ))))))))))
        (when (snd-mixer-selem-has-capture-volume elem)
          (let ((range (mixer-selem-get-capture-volume-range elem)))
            (list
             range
             (cond
               ((plusp (snd-mixer-selem-is-capture-mono elem)) (list (list nil "Mono")))
               (t (iter (for channel from 0 to *snd-mixer-schn-last*)
                        (when (plusp (snd-mixer-selem-has-capture-channel elem channel))
                          (let ((volume (mixer-selem-get-capture-volume elem channel)))
                            (collect (list channel
                                           (snd-mixer-selem-channel-name channel)
                                           volume
                                           (format nil "~d%" (* 100 (/ volume (second range))))
                                           (if (mixer-selem-get-capture-switch elem channel)
                                             "on" "off")
                                           ))))))))))))

(defun mixer-info ()
  (with-mixer (m)
    (snd-mixer-attach m "default")
    (snd-mixer-selem-register m (null-pointer) (null-pointer))
    (snd-mixer-load m)
    (iter (with elem = (snd-mixer-first-elem m))
          (while (not (null-pointer-p elem)))
          (collect (decode-mixer-elem elem))
          (setf elem (snd-mixer-elem-next elem)))))

(defun mixer ()
  "Print audio mixer settings."
  (iter (for (name playback capture) in (mixer-info))
        (princ name)
        (newline)
        (print-table (mapcar #'cdr (cadr playback)) :indent 2 :min-column-width 3)
        (newline)))


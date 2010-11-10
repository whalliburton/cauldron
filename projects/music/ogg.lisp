;; ogg.lisp

(in-package :music)

(defparameter *ogg-sound* "/lisp/projects/music/sounds/rus-ae364135.ogg")

(defun ogg-tags (filename)
  "Open an Ogg Vorbis file, retrieve all the tags, and close it. The
tags are retured as an assoc list with the tag names keywordified."
  (cffi:with-foreign-object (handle 'vorbis-file)
    (vorbis-open filename handle)
     (unwind-protect
          (iter (for tag in (get-vorbis-raw-tags-from-comment (get-vorbis-comment handle)))
                (when (first-iteration-p) (collect (cons :rate (get-vorbis-rate handle))))
                (when-let (pos (position #\= tag))
                  (collect
                      (cons (intern (substitute #\- #\_ (subseq tag 0 pos)) :keyword)
                            (subseq tag (1+ pos) (1- (length tag)))))))
       (vorbis-close handle))))

(defclass music-vorbis-streamer (vorbis-streamer) ())

(defmethod streamer-cleanup :after ((stream music-vorbis-streamer) mixer)
  (declare (ignore mixer))
  (setf *currently-playing* nil))

(defun play-ogg (&optional (filename *ogg-sound*))
  (let* ((tags (ogg-tags filename))
         (rate (cdr (assoc :rate tags)))
         (mixer (create-mixer :rate rate)))
    (let ((streamer (make-vorbis-streamer filename :output-rate rate
                                          :class 'music-vorbis-streamer)))
      (mixer-add-streamer mixer streamer)
      (setf *currently-playing* (list :ogg filename tags mixer)))))

(defun stop-ogg ()
  (destroy-mixer (fourth *currently-playing*)))

;; first.lisp

(in-package :graphics)

(defclass window ()
  ((name :reader name :initarg :name :initform "window")
   (width :reader width :initarg :width :initform (screen-size :width))
   (height :reader height :initarg :height :initform (screen-size :height))
   (context :reader context :initarg :context)))

(defmethod print-object ((window window) stream)
  (with-slots (name) window
    (print-unreadable-object (window stream :type t)
      (format stream "~a" name))))

(defmethod initialize-instance :after ((window window) &rest rest)
  (declare (ignore rest))
  (with-slots (name width height context) window
    (setf context (create-xlib-image-context width height :window-name name))))

(defmethod set-color ((rgb rgb) &optional alpha)
  (if alpha 
    (set-source-rgba (red rgb) (green rgb) (blue rgb) alpha)
    (set-source-rgb (red rgb) (green rgb) (blue rgb))))

(defmethod fill-with-color ((window window) color)
  (let ((*context* (context window)))
    (rectangle 0 0 (width window) (height window))
    (set-color color)
    (fill-path)))

(defclass updating-window (window)
  ((update-function :initarg :update-function :initform 'draw)
   (update-interval :initarg :update-interval :initform 1)
   (update-timer)))

(defmethod initialize-instance :after ((updating-window updating-window) &rest rest)
  (declare (ignore rest))
  (with-slots (update-function update-interval update-timer) updating-window
    (setf update-timer (make-timer (lambda () (funcall update-function updating-window))))
    (schedule-timer update-timer 0 :repeat-interval update-interval)))

(defmethod draw ((window window)))
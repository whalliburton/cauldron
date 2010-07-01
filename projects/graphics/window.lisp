;; first.lisp

(in-package :graphics)

(defclass window ()
  ((name :reader name :initarg :name)
   (width :reader width :initarg :width)
   (height :reader height :initarg :height)
   (context :reader context :initarg :context)))

(defmethod print-object ((window window) stream)
  (with-slots (name) window
    (print-unreadable-object (window stream :type t)
      (format stream "~a" name))))

(defun create-window (name &key (width 500) (height 500))
  (make-instance 'window :name name :width width :height height
                 :context (create-xlib-image-context width height :window-name name)))

(defmethod set-color ((rgb rgb) &optional alpha)
  (if alpha 
    (set-source-rgba (red rgb) (green rgb) (blue rgb) alpha)
    (set-source-rgb (red rgb) (green rgb) (blue rgb))))

(defmethod fill-with-color ((window window) color)
  (let ((*context* (context window)))
    (rectangle 0 0 (width window) (height window))
    (set-color color)
    (fill-path)))

(defvar 2PI (* 2 PI))

(defun demo-window ()
  (let* ((window (create-window "demo"))
         (*context* (context window)))
    (fill-with-color window +magenta4+)
    (select-font-face "Sans" :normal :bold)
    (set-font-size 90)
    (move-to 30 150)
    (set-color +black+)
    (show-text "Hello")
    (set-color +white+)
    (move-to 30 250)
    (text-path "World!")
    (stroke)
    (with-patterns ((pat (create-linear-pattern 200.0 350.0 200.0 450.0)))
      (pattern-add-color-stop-rgba pat 1 0 0 0 1)
      (pattern-add-color-stop-rgba pat 0 1 1 1 1)
      (rectangle 200 350 100 100)
      (arc 100 400 40 0 2PI)
      (set-source pat)
      (fill-path))))


;; xlib.lisp

(in-package :cl-cairo2)

(defcfun ("XDisplayHeight" xdisplayheight) :int 
  (display display)
  (screen-number :int))

(defcfun ("XDisplayWidth" xdisplaywidth) :int 
  (display display)
  (screen-number :int))

(defun screen-size (which)
  (let ((display (xopendisplay (null-pointer))))
    (when (null-pointer-p display)
      (error "Couldn't open display."))
    (multiple-value-prog1
        (case which
          (:width (xdisplaywidth display 0))
          (:height (xdisplayheight display 0)))
        (xclosedisplay display))))

(export 'screen-size)

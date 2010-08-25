;; emacs-test.lisp

(in-package :utilities)

;;; a test of the emacs broadcasting system

(defparameter *emacs-tests*
  '((:weight bold)
    (:slant italic)
    (:foreground "blue")
    (:background "red")
    :underline
    :overline
    :strike-through
    :box
    :inverse-video))

(defun emacs-test ()
  (swank::eval-in-emacs
   `(with-current-buffer (get-buffer-create "*emacs-test*")
      (font-lock-mode t)
      (insert
       ,@(loop for el in *emacs-tests*
               for name = (if (consp el) (car el) el)
               for val = (if (consp el) (cadr el) t)
               nconc
               `((propertize ,(format nil "~(~A~) ~17T~A" name val)
                             'font-lock-face
                             '(,name ,val))
                             ,#\newline)))
      (switch-to-buffer "*emacs-test*")
      nil)))

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

;; from elisp list-colors-display

(defparameter *emacs-colors*
  '("red" "gold" "cyan" "magenta" "dark red"))

(defun emacs-color-test ()
  (swank::eval-in-emacs
   `(with-current-buffer (get-buffer-create "*emacs-colors-test*")
      (font-lock-mode t)
      (insert
       ,@(loop for name in *emacs-colors*
               nconc
               `((propertize ,name
                             'font-lock-face
                             '(:foreground ,name))
                             ,#\newline)))
      (switch-to-buffer "*emacs-colors-test*")
      nil)))

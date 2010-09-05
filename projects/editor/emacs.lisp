;; emacs.lisp

(in-package :editor)

(defun open-in-emacs (filename)
  (swank::eval-in-emacs 
   `(progn (find-file ,filename) nil)))

(defun view-in-emacs (name filename &optional w3m)
  (swank::eval-in-emacs 
   `(let ((buf (generate-new-buffer ,name)))
      (switch-to-buffer buf)
      (insert-file-contents ,filename)
      ,@(when w3m '((w3m-buffer)))
      (setf buffer-read-only t))))

(defun emacs-buffer-list ()
  (swank::eval-in-emacs 
   '(mapcar '(lambda (el) (list (buffer-name el) (buffer-size el))) (buffer-list))))

(defun list-editor-buffers (&optional (sort-on :name))
  (print-table 
   (let ((list (emacs-buffer-list)))
     (case sort-on
       (:name (sort list #'string< :key #'first))
       (:size (sort list #'< :key #'second))))
   :headings '("name" "size")))
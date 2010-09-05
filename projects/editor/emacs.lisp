;; emacs.lisp

(in-package :editor)

(defun open-in-emacs (filename)
  (swank::eval-in-emacs 
   `(progn (find-file ,filename) nil)))

(defun view-in-emacs (name filename)
  (swank::eval-in-emacs 
   `(let ((buf (generate-new-buffer ,name)))
      (switch-to-buffer buf)
      (insert-file-contents ,filename)
      (setf buffer-read-only t))))

(defun view-in-emacs-w3m (filename)
  (swank::eval-in-emacs 
   `(w3m-find-file ,filename)))

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
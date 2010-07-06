;; demo.lisp

(in-package :graphics)

(defun demo-window ()
  (let* ((window (make-instance 'window :name "demo window"))
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

(defun demo-updating-window ()
  (let ((counter 0))
    (make-instance 
     'updating-window :name "demo updating-window"
     :update-function 
     (lambda (window)
       (let ((*context* (context window)))
         (fill-with-color window +magenta4+)
         (select-font-face "Sans" :normal :bold)
         (set-font-size 90)
         (move-to 30 150)
         (set-color +black+)
         (show-text (princ-to-string (incf counter)))
         (stroke))))))



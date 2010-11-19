;; slime.lisp

(in-package :utilities)

(defun start-swank-server (&optional (requested-port 4012))
  "Start a swank server on REQUESTED-PORT. If the port is take it
searches upward for an open port."
  (loop with running = t
        for port = requested-port then (1+ port)
        while running
        do
     (handler-case
         (progn
           (swank:create-server :dont-close t :port port :coding-system "utf-8-unix")
           (format t "SWANK started on ~a~%" port)
           (setf running nil))
       (sb-bsd-sockets:address-in-use-error () nil))))

(let ((original-eval-region))
  (defun naked-eval-region (string)
    (if (and (plusp (length string)) (char= (aref string 0) #\())
      (funcall original-eval-region string)
      (let ((first-word
             (if-let (pos (position #\space string))
               (subseq string 0 pos)
               (string-right-trim '(#\newline) string))))
        (if (fboundp (find-symbol (string-upcase first-word)))
          (funcall original-eval-region (format nil "(~A)" string))
          (funcall original-eval-region string)))))
  (defun use-naked-repl (&optional (enable t))
    (if enable
      (unless original-eval-region
        (setf original-eval-region (symbol-function 'swank::eval-region)
              (symbol-function 'swank::eval-region) #'naked-eval-region))
      (when original-eval-region
        (setf (symbol-function 'swank::eval-region) original-eval-region
              original-eval-region nil)))))

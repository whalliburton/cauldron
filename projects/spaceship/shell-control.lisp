;; shell-control.lisp

(in-package :spaceship)

(defparameter *shell-control-pipe* (in-home "/.spaceship/control"))

(defun ensure-shell-control-pipe ()
  (unless (probe-file *shell-control-pipe*)
    (ensure-directories-exist *shell-control-pipe*)
    (sb-posix:mkfifo *shell-control-pipe* #o700)))

(defun shell-control-monitor-thread ()
  (with-open-file (str *shell-control-pipe*)
    (iter (for line = (read-line str nil))
          (if (null line) (sleep 0.1))
          (when line
            (handle-shell-control line)))))

(defun handle-shell-control (line)
  (let* ((*read-eval* nil)
         (cmd (read-from-string (preprocess-shell-control-line line))))
    (handler-case
        (eval cmd)
      (error (e) (format t "Error in shell control: ~A." e)))))

(defun valid-integer-string (string)
  (handler-case 
      (progn 
        (parse-integer string)
        t)
    (sb-int:simple-parse-error () nil)))

(defun split-on-spaces (string)
  "Split STRING on spaces while ignoring escaped spaces."
  (iter (with length = (length string))
        (with front = 0) 
        (for index from 0 to (1- length))
        (for char = (char string index))
        (for pchar previous char)
        (cond
          ((and (char= char #\space) (char/= pchar #\\))
           (collect (subseq string front index))
           (setf front (1+ index)))
          ((= index (1- length))
           (collect (subseq string front))))))

(defun preprocess-shell-control-line (line)
  (with-output-to-string (stream)
    (iter (for els on (split-on-spaces line))
          (for el = (car els))
          (when (first-iteration-p) (write-char #\( stream))
          (if (or (first-iteration-p) (valid-integer-string el))
            (write-string el stream)
            (progn
              (write-char #\" stream)
              (write-string el stream)
              (write-char #\" stream)))
          (if (cdr els) 
            (write-char #\space stream)
            (write-char #\) stream)))))

(defun start-shell-control-monitor ()
  (ensure-shell-control-pipe)
  (make-thread 'shell-control-monitor-thread :name "shell-control-monitor"))
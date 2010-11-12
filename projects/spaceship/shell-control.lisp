;; shell-control.lisp

(in-package :spaceship)

(defvar *shell-control-pipe* nil)

(defun ensure-shell-control-pipe ()
  (setf *shell-control-pipe*
        (concatenate 'string *spaceship-data-directory* "control"))
  (unless (probe-file *shell-control-pipe*)
    (ensure-directories-exist *shell-control-pipe*)
    (sb-posix:mkfifo *shell-control-pipe* #o700)))

(defun shell-control-monitor-thread ()
  (with-open-file (str *shell-control-pipe*)
    (iter (for line = (read-line str nil))
          (if (null line) (sleep 0.1))
          (when line
            (handler-case
                (handle-shell-control line)
              (error (c)
                (warn "Error in shell-control line ~S. ~A" line c)))))))

(defparameter *shell-control-functions* (all-functions))

(defun handle-shell-control (line)
  (let* ((*read-eval* nil)
         (cmd (read-from-string (preprocess-shell-control-line line))))
    (let* ((output (second cmd))
           (rtn (with-output-to-string (*standard-output*)
                  (handler-case
                      (if-let (fn (caar (member (first cmd) *shell-control-functions*
                                                :key #'car :test #'string-equal)))
                        (let ((*called-from-the-shell* t))
                          (eval (cons fn (cddr cmd))))
                        (format t "No command named: ~A.~%" (first cmd)))
                    (error (e) (format t "Error: ~A~%" e))))))
      (with-open-file (out output :direction :output :if-exists :append)
        (write-string rtn out)))))

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

(defvar *shell-control-monitor-thread* nil)

(defun start-shell-control-monitor ()
  (ensure-shell-control-pipe)
  (setf *shell-control-monitor-thread*
        (make-thread 'shell-control-monitor-thread :name "shell-control-monitor")))

(defun stop-shell-control-monitor ()
  (terminate-thread *shell-control-monitor-thread*))

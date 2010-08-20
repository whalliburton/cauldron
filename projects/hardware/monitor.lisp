;; monitor.lisp

(in-package :hardware)

(defvar *monitor-thread* nil)
(defparameter *monitor-sleep* 1.0)

(defun stop-process-monitor ()
  (when (aand *monitor-thread* (thread-alive-p it))
    (destroy-thread *monitor-thread*)))

(defun start-process-monitor ()
  (stop-process-monitor)
  (setf *monitor-thread* 
        (make-thread 
         (lambda () 
           (iter 
             (process-monitor-heartbeat)
             (sleep *monitor-sleep*)))
         :name "monitor")))

(defvar *monitoring* (make-hash-table))

(defun process-monitor-heartbeat ()
  (iter (for (pid data) in-hashtable *monitoring*)
        (setf (gethash pid *monitoring*)
              (nconc (list (proc-stat pid)) data))))

(defun name-from-cmdline (pid)
  (let ((cmdline (proc-cmdline pid)))
    (cond
      ((string= (first cmdline) "/usr/local/bin/sbcl")
       (iter (for arg in (cdr cmdline))
             (when (string-starts-with arg "(in-package")
               (return (subseq arg 13 (1- (length arg))))))))))

(defun print-monitoring ()
  (print-table 
   (iter (for (pid data) in-hashtable *monitoring*)
         (when (first-iteration-p) 
           (collect (list "name" "pid" "comm" "state" "utime" "stime" "cutime" "cstime")))
         (let ((stat (first data)))
           (with-assoc (comm state utime stime cutime cstime)
             (collect 
                 (list (name-from-cmdline pid)
                       pid (:comm stat) (:state stat) (:utime stat) (:stime stat)
                       (:cutime stat) (:cstime stat))))))))

(defun start-monitoring (name)
  (iter (for pid in (mapcar #'pid (processes-with-name name)))
        (unless (gethash pid *monitoring*)
          (setf (gethash pid *monitoring*) nil))
        (collect pid)))

(defun stop-monitoring (&optional name)
  (if name
    (iter (for pid in (mapcar #'pid (processes-with-name name)))
          (remhash pid *monitoring*))
    (clrhash *monitoring*)))
;; threads.lisp

(in-package :spaceship)

(defun print-threads ()
  (print-table 
   (iter 
     (for thread in (list-all-threads))
     (collect (list (thread-name thread)
                    (if (thread-alive-p thread) "alive" "dead"))))))

(defun worker-thread-p (thread)
  "Return T if THREAD is a worker thread."
  (let ((name (thread-name thread)))
    (and name (string-contains-p name "worker"))))

(defun workers ()
  "Return a list of worker threads."
  (iter (for thread in (list-all-threads))
        (when (worker-thread-p thread)
          (collect thread))))

(defun kill-all-workers ()
  "Destroy all worker threads. Useful for removing hung threads."
  (mapc #'terminate-thread (workers)))

(defun thread-backtrace (thread)
  "Interrupt THREAD and print a backtrace. This should not affect the thread."
  (sb-thread:interrupt-thread 
   thread (lambda () (sb-debug:backtrace 128 *standard-output*))))

(defun thread-break (thread)
  "Interrupt THREAD and break."
  (sb-thread:interrupt-thread thread (lambda () (break))))

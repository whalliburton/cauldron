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


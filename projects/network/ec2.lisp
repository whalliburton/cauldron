;; ec2.lisp

(in-package :network)

(defvar *keys* nil)
(defvar *current-keys* nil)

(defun set-aws-keys (key-set)
  (let ((keys (assoc key-set *keys*)))
    (setf aws:*access-key* (second keys)
          aws:*secret-key* (third keys))
    t))

(defmacro with-keys (&body body)
  (with-gensyms (keys)
    `(iter (for key-set in *current-keys*)
           (for ,keys = (assoc key-set *keys*))
           (let ((aws:*access-key* (second ,keys))
                 (aws:*secret-key* (third ,keys)))
             ,@body))))

(defun instances ()
  "Print out a table of all the running instances."
  (with-keys
    (print-heading (string-downcase key-set))
    (print-table 
     (iter (for instance in (describe-instances))
           (collect (list (get-instance-id instance)
                          (get-public-ip-address instance)
                          (seconds-to-duration-string 
                           (timestamp-difference 
                            (now)
                            (parse-timestring (get-launch-time instance))))
                          (get-key-name instance)))))
    (newline)))

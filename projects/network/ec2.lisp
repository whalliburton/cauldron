;; ec2.lisp

(in-package :network)

(defvar *keys* nil)
(defvar *current-keys* nil)

(defparameter *keys-file* (home "private/ec2-keys.lisp"))

(defun load-ec2-credentials ()
  (if (probe-file *keys-file*)
    (progn
      (load *keys-file*)
      (set-aws-keys (first *current-keys*)))
    (warn "Unable to locate your EC2 credentials.")))

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

(defparameter *protected-instance-ids* '("i-b8c332d0" "i-93bd7ffb"))

(defun protected-instance-id (id)
  (member id *protected-instance-ids* :test #'string-equal))

(defvar *instances* nil)

(defun instances (&optional (refresh t) &key age-limit)
  "Print out a table of all the running instances."
  (with-keys
    (print-heading (string-downcase key-set))
    (print-table
     (iter (for (index instance) in (if (and *instances* (null refresh))
                                      *instances*
                                      (iter (for instance in (describe-instances))
                                            (for index from 1)
                                            (collect (list index instance)))))
           (for launch-time = (get-launch-time instance))
           (for age = (timestamp-difference (now) (parse-timestring launch-time)))
           (when refresh (collect (list index instance) into instances))
           (when (or (null age-limit) (< age age-limit))
             (collect (list index
                            (get-instance-id instance)
                            (get-public-ip-address instance)
                            (seconds-to-duration-string age)
                            (get-key-name instance)
                            (get-state instance)
                            (if (protected-instance-id (get-instance-id instance))
                              "protected" ""))))
           (finally (when refresh (setf *instances* instances)))))
    (newline)))

(defun images (&key architecture)
  (with-keys
    (print-heading (string-downcase key-set))
    (print-table
     (iter (for image in (describe-images))
           (collect
               (nconc
                (when architecture (list (get-architecture image)))
                (list (get-image-id image)
                      (get-manifest image))))))))

(defun zones ()
  (print-table
   (iter (for zone in (describe-zones))
         (collect
             (list (get-zone-name zone)
                   (get-state zone)
                   (get-region zone))))))

(defvar *instance-image-id* nil)
(defvar *instance-key-name* nil)

(defvar *latest-instance* nil)

(defun run-instance ()
  (format t "Starting an instance from image ~A.~%" *instance-image-id*)
  (when-let (instance (run-instances *instance-image-id* *instance-key-name* :instance-type "m1.small"))
    (format t "Instance ~A started on ~A.~%"
            (get-instance-id instance)
            (get-launch-time instance))
    (setf *latest-instance* instance)
    (get-instance-id instance)))

(defmacro with-instance-from-index ((index) &body body)
  (once-only (index)
    `(if-let (instance (second (assoc ,index *instances* :test #'=)))
       (let ((instance-id (get-instance-id instance))
             (ip (get-public-ip-address instance)))
         (declare (ignorable instance-id))
         ,@body)
       (error "There is no instance at index ~A." ,index))))

(defun terminate-instance (&rest instance-indexes)
  (iter (for instance-index in instance-indexes)
        (with-instance-from-index (instance-index)
          (if (protected-instance-id instance-id)
            (warn "Instance ~A is protected from termination." instance-id)
            (progn
              (format t "Terminating instance ~A.~%" instance-id)
              (terminate-instances instance-id))))))

(defun terminate (&rest instance-indexes) (apply 'terminate-instance instance-indexes))

(defun console (instance-index)
  (with-instance-from-index (instance-index)
    (format t "Printing the console output from instance ~A (~A).~%" instance-id instance-index)
    (write-string (substitute #\space #\return (get-console-output instance-id)))))

(defun ssh (instance-index &key (user "root") (agent-forwarding t))
  (with-instance-from-index (instance-index)
    (let ((host (format nil "~A@~A" user (get-public-ip-address instance))))
      (format t "SSHing to ~A ~A (~A).~%" host instance-id instance-index)
      (ssh-in-emacs host :agent-forwarding agent-forwarding))))

(defun send-file (filename instance-index &optional (user "root") remote-filename)
  (unless (probe-file filename)
    (error "No file found at ~A." filename))
  (with-instance-from-index (instance-index)
    (let ((host (format nil "~A@~A:~@[~A~]" user (get-public-ip-address instance)
                        remote-filename)))
      (scp filename host))))

(defun send-string-as-file (string instance-index filename &optional (user "root"))
  (with-temporary-file (stream tmpname)
    (write-string string stream)
    (force-output stream)
    (send-file tmpname instance-index user filename)))

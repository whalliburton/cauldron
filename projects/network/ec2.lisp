;; ec2.lisp

(in-package :network)

(defparameter *keys-file* (home "private/ec2-keys.lisp"))

(defparameter *protected-instance-ids* '("i-b8c332d0" "i-93bd7ffb"))

(defparameter *keys* nil)

(defparameter *current-key* nil)

(defparameter *instance-image-id* nil "The default image ID used by RUN-INSTANCE.")

(defparameter *instance-key-names* nil "The key name used by RUN-INSTANCE.")

(defun protected-instance-id (id)
  (member id *protected-instance-ids* :test #'string-equal))

(defvar *instances* nil)

(defvar *hidden-instance-ids* (make-hash-table :test 'equal))

(defun is-hidden (id)
  (gethash id *hidden-instance-ids*))

(defun load-ec2-credentials (&optional (credentials-file *keys-file*))
  (when (probe-file credentials-file)
    (load credentials-file)
    (set-repl-keys *current-key*)))

(defun set-repl-keys (key-set)
  (let ((keys (assoc key-set *keys*)))
    (setf aws:*access-key* (second keys)
          aws:*secret-key* (third keys))
    t))

(defmacro with-current-key (&body body)
  (with-gensyms (keys)
    `(let* ((,keys (assoc *current-key* *keys*))
            (aws:*access-key* (second ,keys))
            (aws:*secret-key* (third ,keys)))
       ,@body)))

(defvar *image-names* (make-instance 'expiring-hash-table :test 'equal
                                     :timeout (* 60 60 24)))

(defun instances (&key (refresh t)  age-limit (show-ami t) (show-type t)
                       (types-as-names t))
  "List the running and recently terminated instances."
  (with-current-key
    (print-table
     (iter (for (index instance) in (if (and *instances* (null refresh))
                                      *instances*
                                      (iter (for instance in (describe-instances))
                                            (for index from 1)
                                            (collect (list index instance)))))
           (unless (is-hidden (get-instance-id instance))
             (for launch-time = (get-launch-time instance))
             (for age = (timestamp-difference (now) (parse-timestring launch-time)))
             (when refresh (collect (list index instance) into instances))
             (when (or (null age-limit) (< age age-limit))
               (collect (nconc
                         (list index
                               (get-instance-id instance)
                               (or (get-public-ip-address instance) "")
                               (seconds-to-duration-string age)
                               (get-key-name instance)
                               (get-state instance))
                         (when show-ami (list
                                         (or (and types-as-names
                                                  (gethash-expiring (get-image-id instance)
                                                                    *image-names*))
                                             (subseq (get-image-id instance) 4))))
                         (when show-type (list (get-instance-type instance)))
                         (list (if (protected-instance-id (get-instance-id instance))
                                 "protected" ""))))))
           (finally (when refresh (setf *instances* instances)))))))

(defalias i instances)

(defun name-from-manifest (manifest)
  (subseq manifest (1+ (position #\/ manifest)) (position #\. manifest)))

(defun ensure-string (el)
  (if (null el) ""
    (typecase el
      (symbol (symbol-name el))
      (string el)
      (t (princ-to-string el)))))

(defun use-image (id-or-name)
  "Set the image used to start new instances."
  (let ((id-or-name (ensure-string id-or-name)))
    (if (string-starts-with "ami-" id-or-name)
      (setf *instance-image-id* id-or-name)
      (progn
        (iter (for image in (describe-images))
              (let ((prefix (format nil "wch/~A" id-or-name)))
                (when (string-starts-with (get-manifest image) prefix :test #'char-equal)
                  (setf *instance-image-id* (get-image-id image))
                  (return-from use-image *instance-image-id*))))
        (error "No image with id or name of ~A." id-or-name))))
  *instance-image-id*)

(defun images (&key architecture manifest selected-only)
  "List all the disk images used to start new instances."
  (with-current-key
    (print-table
     (sort
      (iter (for image in (describe-images))
            (for selected = (equal *instance-image-id* (get-image-id image)))
            (when (or (null selected-only) selected)
              (collect
                  (nconc
                   (when architecture (list (get-architecture image)))
                   (let ((name (name-from-manifest (get-manifest image)))
                         (image-id (get-image-id image)))
                     (set-expiring-hash *image-names* image-id name)
                     (nconc
                      (list name)
                      (list image-id)
                      (when manifest (list (get-manifest image)))
                      (list (if selected "** selected **" ""))))))))
      #'string< :key #'car))))

(defun zones ()
  (print-table
   (iter (for zone in (describe-zones))
         (collect
             (list (get-zone-name zone)
                   (get-state zone)
                   (get-region zone))))))

(defvar *instance-key-names* nil)

(defvar *latest-instance* nil)

(defun run-instance (&key (number 1) (instance-type "m1.small")
                          (image-id *instance-image-id*))
  "Launch a new instance."
  (with-current-key
    (format t "Starting ~:[an instance~;multiple instances~] from image ~A~@[ of type ~S~].~%"
            (< 1 number)
            image-id
            (gethash-expiring image-id *image-names*))
    (when-let (instance (run-instances image-id
                                       (second (assoc *current-key* *instance-key-names*))
                                       :instance-type instance-type
                                       :mincount (princ-to-string number)
                                       :maxcount (princ-to-string number)))
      (format t "Instance ~A started on ~A.~%"
              (get-instance-id instance)
              (get-launch-time instance))
      (setf *latest-instance* instance)
      (get-instance-id instance))))

(defalias run run-instance)

(defun run-big (&optional (number 1)) (run-instance :number number :instance-type "c1.medium"))

(defmacro with-instance-from-index ((index) &body body)
  (once-only (index)
    `(if-let (instance (second (assoc ,index *instances* :test #'=)))
       (let ((instance-id (get-instance-id instance))
             (ip (get-public-ip-address instance)))
         (declare (ignorable instance-id ip))
         ,@body)
       (error "There is no instance at index ~A." ,index))))

(defmacro iter-instance-index ((indexes) &body body)
  `(iter (for instance-index in ,indexes)
        (with-instance-from-index (instance-index)
          ,@body)))

(defun terminate-instance (&rest instance-indexes)
  "Terminate an instance."
  (iter-instance-index (instance-indexes)
    (if (protected-instance-id instance-id)
      (warn "Instance ~A is protected from termination." instance-id)
      (progn
        (format t "Terminating instance ~A.~%" instance-id)
        (with-current-key
          (terminate-instances instance-id))))))

(defalias terminate terminate-instance)

(defun console (&rest instance-indexes)
  "Print out the console for the instance."
  (with-current-key
    (iter-instance-index (instance-indexes)
      (format t "Printing the console output from instance ~A (~A).~%" instance-id instance-index)
      (let ((text (substitute #\space #\return (get-console-output instance-id))))
        (if text
          (write-string text)
          (format t "No console output."))))))

(defun ssh (instance-index &key (user "root"))
  "Open a new shell on the instance."
  (with-instance-from-index (instance-index)
    (let ((host (format nil "~A@~A" user (get-public-ip-address instance))))
      (format t "SSHing to ~A ~A (~A).~%" host instance-id instance-index)
      (ssh-in-emacs host :agent-forwarding t))))

(defun protect (&rest instance-indexes)
  "Protect the instance from termination."
  (iter-instance-index (instance-indexes)
    (push instance-id *protected-instance-ids*)
    (format t "Protecting instance ~A.~%" instance-id)))

(defun unprotect (&rest instance-indexes)
  "Allow the instance to be terminated."
  (iter-instance-index (instance-indexes)
    (setf *protected-instance-ids*
          (remove instance-id *protected-instance-ids* :test #'string-equal))
    (format t "*** UNPROTECTING instance ~A ***~%" instance-id)))

(defun hide (&rest instance-indexes)
  "Hide the instance from the listings."
  (iter-instance-index (instance-indexes)
    (setf (gethash instance-id *hidden-instance-ids*) t)
    (format t "Hiding instance ~A.~%" instance-id)))

(defun hide-terminated ()
  "Hide all the terminated instances from the listings."
  (iter (for instance in (describe-instances))
        (when (string= (get-state instance) "terminated")
          (setf (gethash (get-instance-id instance) *hidden-instance-ids*) t))))

(defun show (&rest instance-indexes)
  "Show a previously hidden instance in the listings."
  (iter-instance-index (instance-indexes)
    (remhash instance-id *hidden-instance-ids*)
    (format t "Sowing instance ~A.~%" instance-id)))

(defun send-file (filename instance-index &optional (user "root") remote-filename)
  "Send a local file to an instance."
  (unless (probe-file filename)
    (error "No file found at ~A." filename))
  (with-instance-from-index (instance-index)
    (let ((host (format nil "~A@~A:~@[~A~]" user (get-public-ip-address instance)
                        remote-filename)))
      (scp filename host))))

(defun send-string (string instance-index filename &optional (user "root"))
  "Send a string to an instance as a file."
  (with-temporary-file (stream tmpname)
    (write-string string stream)
    (force-output stream)
    (send-file tmpname instance-index user filename)))

(defun view-instance (&rest instance-indexes)
  "View the instance in a web browser."
  (iter-instance-index (instance-indexes)
    (run-program "/usr/bin/chromium" (list (format nil "http://~A" ip)) )))

(defalias view view-instance)

(defun shorten-lisp (name)
  (if (string-starts-with name "/usr/local/bin/")
    (subseq name 15)
    name))

(defun list-lisps (&rest instance-indexes)
  "List the running lisp processes on an instance."
  (iter-instance-index (instance-indexes)
    (newline)
    (print-table
     (iter (for line in (with-instance-from-index (instance-index)
                          (ssh-command-collect "ps au | grep sbcl | grep -v grep" ip)))
           (for all = (split-sequence #\space line :remove-empty-subseqs t))
           (when (first-iteration-p) (collect (list "")))
           (collect (subseq all 0 10))
           (collect (list :subtable
                          (list
                           (prin1-with-ellipses-to-string
                            (shorten-lisp
                             (apply #'concatenate 'string (intersperse (subseq all 10) " ")))
                            95 'princ-to-string))))
           (collect (list "")))
     :headings '("user" "pid" "%cpu" "%mem" "vsz" "rss" "tty" "state" "start" "time"))))

(defun split-on-spaces (string)
  (split-sequence #\space string :remove-empty-subseqs t))

(defun top (&rest instance-indexes)
  "Print the top CPU using processes on an instance."
  (iter-instance-index (instance-indexes)
    (let* ((lines (ssh-command-collect "top -b -n 1" ip))
           (processes (subseq lines 5)))
      (print-table (subseq (mapcar 'split-on-spaces processes) 1 5)))))

(defun top-full (&rest instance-indexes)
  "Print out information on all the instance's processes."
  (iter-instance-index (instance-indexes)
    (when (plural instance-indexes)
      (print-heading (format nil "top on ~a" instance-id)))
    (let* ((lines (ssh-command-collect "top -b -n 1" ip))
           (headers (subseq lines 0 5))
           (processes (subseq lines 5)))
      (mapc (lambda (el) (format t "~A~%" el)) headers)
      (print-table (mapcar 'split-on-spaces processes)))))

(defun ps (&rest instance-indexes)
  "Print a listing of all the instance's processes."
  (iter-instance-index (instance-indexes)
    (let ((lines (ssh-command-collect "ps aux" ip)))
      (print-table
       (iter (for line in lines)
             (for columns = (subseq (split-on-spaces (subseq line 0 65)) 0 2))
             (for cmd = (string-trim '(#\space #\tab) (subseq line 65)))
             (collect (nconc columns (list cmd))))))))

(defun ensure-image-id (id)
  (or (maphash-expiring
       (lambda (ami name)
         (when (string= id name)
           (return-from ensure-image-id ami)))
       *image-names*)
      id))

(defun remove-image (&rest image-ids)
  "Remove an image from the listing."
  (iter (for image-id in image-ids)
        (ec2:issue-request
         `(("Action" . "DeregisterImage") ("ImageId" . ,(ensure-image-id image-id))))))

(defun register-image (name manifest)
  (ec2:issue-request
   `(("Action" . "RegisterImage") ("Name" . ,name) ("ImageLocation" . ,manifest))))

(defun modify-launch-permissions (image-id user-id &optional (operation "add"))
  (ec2:issue-request
   `(("Action" . "ModifyImageAttribute")
     ("ImageId" . ,image-id)
     ("UserId" . ,user-id)
     ("Attribute" . "launchPermission")
     ("OperationType" . ,operation))))

(defun authorize-ingress (port &key (group "default") (protocol "tcp"))
  (ec2:issue-request
   `(("Action" . "AuthorizeSecurityGroupIngress")
     ("GroupName" . ,group)
     ("IpPermissions.1.IpProtocol" . ,protocol)
     ("IpPermissions.1.FromPort" . ,port)
     ("IpPermissions.1.ToPort" . ,port)
     ("IpPermissions.1.IpRanges.1.CidrIp" . "0.0.0.0/0"))))

;; devices.lisp

(in-package :hardware)

;;; from stumpwm

(defun read-line-from-sysfs (stream &optional (blocksize 80))
  "READ-LINE, but with a workaround for a known SBCL/Linux bug
regarding files in sysfs. Data is read in chunks of BLOCKSIZE bytes."
  (let ((buf (make-array blocksize
			 :element-type '(unsigned-byte 8)
			 :initial-element 0))
	(fd (sb-sys:fd-stream-fd stream))
	(string-filled 0)
	(string (make-string blocksize))
	bytes-read
	pos
	(stringlen blocksize))

    (loop
       ;; Read in the raw bytes
       (setf bytes-read
	     (sb-unix:unix-read fd (sb-sys:vector-sap buf) blocksize))

       ;; Why does SBCL return NIL when an error occurs?
       (when (or (null bytes-read)
                 (< bytes-read 0))
         (error "UNIX-READ failed."))

       ;; This is # bytes both read and in the correct line.
       (setf pos (or (position (char-code #\Newline) buf) bytes-read))

       ;; Resize the string if necessary.
       (when (> (+ pos string-filled) stringlen)
	 (setf stringlen (max (+ pos string-filled)
			      (* 2 stringlen)))
	 (let ((new (make-string stringlen)))
	   (replace new string)
	   (setq string new)))

       ;; Translate read bytes to string
       (setf (subseq string string-filled)
	     (sb-ext:octets-to-string (subseq buf 0 pos)))

       (incf string-filled pos)

       (if (< pos blocksize)
	   (return (subseq string 0 string-filled))))))

(defun read-sysfs-field (path field-name)
 (with-open-file 
     (file (merge-pathnames (make-pathname :name field-name) path))
   (read-line-from-sysfs file)))

;;; original

(defun device-classes ()
  (mapcar (lambda  (el) (intern (string-upcase (last1 (pathname-directory el))) :keyword))
          (list-directory "/sys/class")))

(defclass device ()
  ((name :initarg :name :reader name)
   (path :initarg :path :reader path)
   (class :initarg :class)))

(defmethod sysfs-field ((device device) field-name &optional (as 'string))
  (let ((val (read-sysfs-field (path device) field-name)))
    (ecase as
      (string val)
      (integer (parse-integer val :junk-allowed t)))))

(defmethod print-object ((device device) stream)
  (with-slots (name class) device
    (print-unreadable-object (device stream :type t) 
      (format stream "~a ~a" class name))))

(defclass power-supply (device) 
  ((type :initarg :type)))

(defmethod print-object ((power-supply power-supply) stream)
  (with-slots (name type) power-supply
    (print-unreadable-object (power-supply stream :type t) 
      (format stream "~a ~a ~(~a~)" name type 
              (case (sysfs-field power-supply "online" 'integer)
                (1 :online) (0 :offline))))))

(defmethod initialize-instance :after ((power-supply power-supply) &rest rest)
  (declare (ignore rest))
  (setf (slot-value power-supply 'type) (sysfs-field power-supply "type")))

(defclass battery (power-supply) ())

(defmethod battery-percentage ((battery battery))
  (* 100 (/ (sysfs-field battery "energy_now" 'integer)
            (sysfs-field battery "energy_full" 'integer))))

(defmethod battery-status ((battery battery))
  (sysfs-field battery "status"))

(defmethod print-object ((battery battery) stream)
  (with-slots (name type) battery
    (print-unreadable-object (battery stream :type t) 
      (format stream "~a ~,2f% ~a" name (battery-percentage battery) (battery-status battery)))))

(defclass input (device)
  (name))

(defmethod initialize-instance :after ((input input) &rest rest)
  (declare (ignore rest))
  (setf (slot-value input 'name) (sysfs-field input "name")))

(defmethod print-object ((input input) stream)
  (with-slots (name) input
    (print-unreadable-object (input stream :type t) 
      (format stream "~a" name))))

(defun class-from-sysfs-class-path (class path)
  (case class
    (:power_supply 
       (let ((type (read-sysfs-field path "type")))
         (cond 
           ((string= type "Battery") 'battery)
           (t 'power-supply))))
    (:input 
       (if (string-starts-with (last1 (pathname-directory path)) "input")
         'input
         'device))
    (t 'device)))

(defun split-at-integer-suffix (string)
  (iter (for x from (1- (length string)) downto 0)
        (for is-digit = (digit-char-p (char string x)))
        (for was-digit previous is-digit)
        (while is-digit)
        (finally (return (when was-digit 
                           (list
                             (subseq string 0 (1+ x))
                             (parse-integer string :start (1+ x))))))))

(defun group (list key-fn)
  (let ((hash (make-hash-table :test 'equal)))
   (mapc
     (lambda (el) 
       (let* ((key (funcall key-fn el))
              (val (gethash key hash)))
         (setf (gethash key hash) (cons el val))))
     list)
    hash))

(defun shorten-devices (devices)
  (iter (for (k v) in-hashtable
             (group 
              devices
              (lambda (el) 
                 (first (split-at-integer-suffix (slot-value el 'name))))))
        (cond 
          ((null k) (when (null k)
                      (nconcing (mapcar #'(lambda (el) (slot-value el 'name)) v))))
          ((null (cdr v)) (collect (slot-value (first v) 'name)))
          (t  (collect (concatenate 'string
                                    k
                                    (range-description
                                     (iter (for el in v)
                                           (collect (second 
                                                     (split-at-integer-suffix
                                                      (slot-value el 'name))))))))))))

(defun range-description (list)
  "(range-description '(1 3 4 6 7 8 9 10)) -> \"[1,3-4,6-10]\""
  (let ((list (sort list #'<)))
    (with-output-to-string (stream)
      (write-char #\[ stream)
      (iter (with (inside leading))
            (for els on list)
            (for el = (car els))
            (for last previous el)
            (cond
              ((not inside) 
               (princ el stream)
               (setf inside t leading el))
              ((null (cdr els))
               (if (= last (1- el))
                 (progn
                   (if (eql leading (1- el))
                     (write-char #\, stream)
                     (write-char #\- stream))
                   (princ el stream))
                 #1=(progn
                      (unless (eq last leading)
                        (if (eql leading (1- last))
                          (write-char #\, stream)
                          (write-char #\- stream))
                        (princ last stream))
                      (write-char #\, stream)
                      (princ el stream))))
              ((and inside (/= last (1- el)))
               #1#
               (setf leading el))))
      (write-char #\] stream))))

(defun list-devices (&optional class)
  "List all the devices of CLASS. If CLASS is NIL, list all the devices."
  (if (null class)
    (mapcan #'list-devices (device-classes))
    (let ((path (format nil "/sys/class/~(~a~)" class)))
      (unless (probe-file path)
        (error "SYSFS class ~a does not exists." class))
      (mapcar (lambda (el) 
                (make-instance (class-from-sysfs-class-path class el)
                               :name (last1 (pathname-directory el)) 
                               :path el
                               :class class))
              (list-directory path)))))

(defun devices ()
  "List all the devices on this machine."
  (print-table 
   (iter (for (class devices) in-hashtable 
              (group (list-devices) (lambda (el) (slot-value el 'class))))
         (nconcing (iter (for name in (sort (shorten-devices devices) #'string<))
                         (collect (list name (string-downcase class))))))))

(defun list-batteries ()
  (iter (for device in (list-devices :power_supply))
        (when (typep device 'battery)
          (collect device))))

(defun battery (&key as-list)
  "Print out the battery status."
  (let ((list
         (iter (for battery in (list-batteries))
               (collect (list (name battery) 
                              (format nil "~,2f%" (battery-percentage battery))
                              (battery-status battery))))))
    (if as-list list (print-table list))))

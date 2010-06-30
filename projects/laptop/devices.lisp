;; devices.lisp

(in-package :laptop)

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
  ((name :initarg :name)
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

(defun class-from-sysfs-class-path (class path)
  (case class
    (:power_supply 
       (let ((type (read-sysfs-field path "type")))
         (cond 
           ((string= type "Battery") 'battery)
           (t 'power-supply))))
    (t 'device)))

(defun devices (&optional class)
  "List all the devices of CLASS. If CLASS is NIL, list all the devices."
  (if (null class)
    (mapcan #'devices (device-classes))
    (let ((path (format nil "/sys/class/~(~a~)" class)))
      (unless (probe-file path)
        (error "SYSFS class ~a does not exists." class))
      (mapcar (lambda (el) 
                (make-instance (class-from-sysfs-class-path class el)
                               :name (last1 (pathname-directory el)) 
                               :path el
                               :class class))
              (list-directory path)))))
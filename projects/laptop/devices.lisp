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

(defun sysfs-field-exists? (path name)
  (probe-file (merge-pathnames (make-pathname :name name) path)))

(defun sysfs-field (path name)
  (with-open-file (file (merge-pathnames (make-pathname :name name)  path))
    (read-line-from-sysfs file)))

(defun sysfs-int-field (path name)
  (parse-integer (sysfs-field path name) :junk-allowed t))

(defun sysfs-int-field-or-nil (path name)
  (if (sysfs-field-exists? path name)
    (sysfs-int-field path name)
    nil))

;;; original

(defun device-classes ()
  (mapcar (lambda  (el) (intern (string-upcase (last1 (pathname-directory el))) :keyword))
          (list-directory "/sys/class")))

(defclass device ()
  ((name :initarg :name :reader name)
   (path :initarg :path :reader path)
   (class :initarg :class :reader device-class)))

(defmethod print-object ((device device) stream)
  (with-slots (name class) device
    (print-unreadable-object (device stream :type t) 
      (format stream "~a ~a" class name))))

(defun devices (&optional class)
  "List all the devices of CLASS. If CLASS is NIL, list all the devices."
  (if (null class)
    (mapcan #'devices (device-classes))
    (let ((path (format nil "/sys/class/~(~a~)" class)))
      (unless (probe-file path)
        (error "SYSFS class ~a does not exists." class))
      (mapcar (lambda (el) 
                (make-instance 'device 
                               :name (last1 (pathname-directory el)) 
                               :path el
                               :class class))
              (list-directory path)))))
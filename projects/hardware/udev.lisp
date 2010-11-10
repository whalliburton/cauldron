;; udev.lisp

(in-package :hardware)

(load-foreign-library "/usr/lib/libudev.so")

(defcfun "udev_new" :pointer)
(defcfun "udev_unref" :void (udev :pointer))

(defmacro with-udev ((var) &body body)
  `(let ((,var (udev-new)))
     (unwind-protect
          (progn ,@body)
       (udev-unref ,var))))

;;; monitor

(defcfun "udev_monitor_new_from_netlink" :pointer (udev :pointer) (name :string))
(defcfun "udev_monitor_filter_add_match_subsystem_devtype" :int
  (monitor :pointer) (subsystem :string) (devtype :string))
(defcfun "udev_monitor_enable_receiving" :int (monitor :pointer))
(defcfun "udev_monitor_receive_device" :pointer (monitor :pointer))

;;; device

(defcfun "udev_device_get_properties_list_entry" :pointer (device :pointer))

;;; list-entry

(defcfun "udev_list_entry_get_next" :pointer (list-entry :pointer))
(defcfun "udev_list_entry_get_name" :string (list-entry :pointer))
(defcfun "udev_list_entry_get_value" :string (list-entry :pointer))

(defun list-entry-to-list (list-entry)
  (iter (while (not (null-pointer-p list-entry)))
        (collect (cons (udev-list-entry-get-name list-entry)
                       (udev-list-entry-get-value list-entry)))
        (setf list-entry (udev-list-entry-get-next list-entry))))

;;; enumerate

(defcfun "udev_enumerate_new" :pointer (udev :pointer))
(defcfun "udev_enumerate_unref" :void (enumerate :pointer))
(defcfun "udev_enumerate_scan_subsystems" :int (enumerate :pointer))
(defcfun "udev_enumerate_scan_devices" :int (enumerate :pointer))
(defcfun "udev_enumerate_get_list_entry" :pointer (enumerate :pointer))

(defmacro with-enumerate ((var udev) &body body)
  `(let ((,var (udev-enumerate-new ,udev)))
     (unwind-protect
          (progn ,@body)
       (udev-enumerate-unref ,var))))

(defun list-subsystems ()
  (flatten
   (with-udev (udev)
     (with-enumerate (enum udev)
       (udev-enumerate-scan-subsystems enum)
       (list-entry-to-list (udev-enumerate-get-list-entry enum))))))

(defun list-udev-devices ()
  (flatten
   (with-udev (udev)
     (with-enumerate (enum udev)
       (udev-enumerate-scan-devices enum)
       (list-entry-to-list (udev-enumerate-get-list-entry enum))))))

;;; monitor thread

(defvar *udev-monitor-thread* nil)
(defparameter *udev-monitor-subsystems* '("block" "usb"))
(defvar *udev-messages* (make-mailbox))

(defun start-udev-monitor ()
  (when (and *udev-monitor-thread* (thread-alive-p *udev-monitor-thread*))
    (error "The UDEV monitor is already running."))
  (format t "Starting the udev monitor on subsystems: ~{~A~^ ~}~%" *udev-monitor-subsystems*)
  (setf *udev-monitor-thread* (make-thread 'udev-monitor-thread :name "udev-monitor" )))

(defun restart-udev-monitor ()
  (unless (thread-alive-p *udev-monitor-thread*) (error "The UDEV monitor is not running."))
  ;;; does not properly clean up
  (destroy-thread *udev-monitor-thread*)
  (sleep 1)
  (start-udev-monitor))

(defun udev-monitor-thread ()
  (let ((monitor (udev-monitor-new-from-netlink (udev-new) "udev")))
    (iter (for subsystem in *udev-monitor-subsystems*)
          (udev-monitor-filter-add-match-subsystem-devtype monitor subsystem (null-pointer)))
    (udev-monitor-enable-receiving monitor)
    (iter (send-message
           *udev-messages*
           (list-entry-to-list
            (udev-device-get-properties-list-entry
             (udev-monitor-receive-device monitor)))))))

(defun print-pending-udev-messages ()
  (iter (for message in (receive-pending-messages *udev-messages*))
        (iter (for el in message)
              (format t "~20a ~a~%" (car el) (cdr el)))
        (newline)))

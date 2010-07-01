;; udev.lisp

(in-package :laptop)

(load-foreign-library "/usr/lib/libudev.so")

(defcfun "udev_new" :pointer)

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

;;; monitor thread

(defvar *udev-monitor-thread* nil)
(defvar *udev-monitor-subsystems* '("block"))
(defvar *udev-messages* (make-mailbox))

(defun start-udev-monitor ()
  (when *udev-monitor* (error "The UDEV monitor is already running."))
  (make-thread 'udev-monitor-thread :name "udev-monitor" ))

(defun udev-monitor-thread ()
  (let* ((udev (udev-new))
         (monitor (udev-monitor-new-from-netlink udev "udev")))
    (iter (for subsystem in *udev-monitor-subsystems*)
          (udev-monitor-filter-add-match-subsystem-devtype monitor subsystem (null-pointer)))
    (udev-monitor-enable-receiving monitor)
    (iter (send-message 
           *udev-messages* 
           (list-entry-to-list
            (udev-device-get-properties-list-entry 
             (udev-monitor-receive-device monitor)))))))

;; scanner.lisp

(in-package :hardware)

(load-foreign-library "/usr/lib/libsane.so")

(defcenum sane-status
  :good :unsupported :cancelled :device-busy :invalid :eof 
  :jammed :no-docs :cover-open :io-error
  :no-mem :access-denied)

(defcfun sane-init sane-status (version-code :pointer) (authorization-callback :pointer))
(defcfun sane-exit :void)

(defcfun sane-get-devices sane-status (device-list :pointer) (local-only :int))

(defcstruct sane-device
  (name :string)
  (vendor :string)
  (model :string)
  (type :string))

(define-condition failed-to-detect-sane-divices (error)
  ((return-code :initarg :return-code)))

(defun sane-list-devices ()
  "List detected SANE devices as a list of (name vendor model type)."
  (with-foreign-object (pointer :pointer)
    (let ((rtn (sane-get-devices pointer 0)))
      (if (eq rtn :good)
        (iter (for x upfrom 0)
              (for device-pointer = (mem-aref (mem-ref pointer :pointer) :pointer x))
              (while (not (null-pointer-p device-pointer)))
              (collect 
                  (with-foreign-slots ((name vendor model type) device-pointer sane-device)
                    (list name vendor model type))))
        (error 'failed-to-detect-sane-devices :return-code rtn)))))

(define-condition failed-to-open-sane-device (error)
  ((name :initarg :name) 
   (return-code :initarg :return-code)))

(defcfun ("sane_open" %sane-open) sane-status (name :string) (handle :pointer))

(defun sane-open (name)
  (with-foreign-object (pointer :pointer)
    (let ((rtn (%sane-open name pointer)))
      (if (eq rtn :good)
        (mem-ref pointer :pointer)
        (error 'failed-to-open-sane-device :name name :return-code rtn)))))

(defcfun sane-close :void (handle :pointer))

(defcfun sane-start sane-status (handle :pointer))
(defcfun sane-cancel :void (handle :pointer))

(defcenum sane-value-type :bool :int :fixed :string :button :group)
(defcenum sane-unit :none :pixel :bit :mm :dpi :percent :microsecond)

(defcstruct sane-option
  (name :string)
  (title :string)
  (desc :string)
  (type sane-value-type)
  (unit sane-unit)
  (size :int)
  (cap :int)
  (constraint :pointer))

(defcfun sane-get-option-descriptor :pointer (handle :pointer) (n :int))

(defcenum sane-action :get-value :set-value :set-auto)
(defbitfield sane-info (:inexact #x01) :reload-options :reload-params)

(defcfun sane-control-option sane-status (handle :pointer) (n :int) (action sane-action)
         (v :pointer) (i :pointer))

(defun sane-get-option (handle index)
  (let ((option (sane-get-option-descriptor handle index)))
    (unless (null-pointer-p option)
      (with-foreign-slots ((name title desc type unit size) option sane-option)
        (with-foreign-pointer (pointer size)
          (sane-control-option handle index :get-value pointer (null-pointer))
          (values (case type
                    (:int (mem-ref pointer :int))
                    (:fixed (mem-ref pointer :int))
                    (:string (foreign-string-to-lisp pointer))
                    (:bool (= 1 (mem-ref pointer :int)))) 
                  name unit title desc))))))

(defun sane-get-options (handle)
  (iter (for index from 1 to (1- (sane-get-option handle 0)))
        (multiple-value-bind (value name unit title desc) (sane-get-option handle index)
          (declare (ignore name unit desc))
          (collect (list title value)))))

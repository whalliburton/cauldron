;; send-file.lisp

(in-package :communications)

;; we could use FFI and http://www.libssh2.org/examples/scp.html but
;; that would add a libssh2 dependency while 'scp' is probably
;; installed on most machines

(define-condition failure-to-scp (error)
  ((message :initarg :message)))

(defun scp (from to)
  (with-child-process (process (format nil "/usr/bin/scp ~a ~a" from to) :stderr t)
    (multiple-value-bind (pid return-value) (process-wait process)
      (declare (ignore pid))
      (unless (zerop return-value)
        (error 'failure-to-scp :message
               (string-trim '(#/return) (read-line (process-error process)))))
      t)))

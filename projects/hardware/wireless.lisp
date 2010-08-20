;; wireless.lisp

(in-package :hardware)

(load-foreign-library "/lib64/libiw.so")

(defcfun iw-sockets-open :int)
(defun iw-sockets-close (skfd) (sb-posix:close skfd))

(defcfun iw-enum-devices :void (skfd :int) (handler :pointer) (args :pointer) (count :int))

(defmacro with-iw-socket ((skfd) &body body)
  `(let ((,skfd (iw-sockets-open)))
     (unwind-protect 
          (progn ,@body)
       (iw-sockets-close ,skfd))))

(defun network-devices ()
  (with-iw-socket (skfd)
    (let ((rtn))
      (defcallback wireless-enumerator :int ((skfd :int) (ifname :string) (args :pointer) (count :int))
        (declare (ignore skfd args count))
        (push ifname rtn) 0)
      (iw-enum-devices skfd (callback wireless-enumerator) (null-pointer) 1)
      rtn)))


;; random.lisp

(in-package :utilities)

(let ((random-context (isaac:init-kernel-seed)))
  (defun random-string (&optional (bytes 16))
    "Returns a random number (as a hexadecimal string) of BYTES bytes."
    (format nil (format nil "~~~D,'0x" (* bytes 2))
            (isaac:rand-bits random-context (* bytes 8))))

  (defun random-byte-vector (&optional (bytes 16))
    "Returns a vector of BYTES random bytes. BYTES must be a multiple of 4."
    (assert (zerop (mod bytes 4)))
    (make-array bytes :element-type '(unsigned-byte 8)
                :initial-contents
                (iter (repeat (/ bytes 4))
                      (let ((val (isaac:rand32 random-context)))
                        (nconcing (list
                                   (ldb (byte 8 0) val)
                                   (ldb (byte 8 8) val)
                                   (ldb (byte 8 16) val)
                                   (ldb (byte 8 24) val))))))))


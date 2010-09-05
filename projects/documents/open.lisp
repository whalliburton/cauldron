;; open.lisp

(in-package :documents)

(defun open-document (which)
  "Open for reading any readable file or unarchive any archive."
  (etypecase which
    (string 
       (if (probe-file which)
         (string-case
             ((let ((mime (magic-mime which)))
                (warn "mime ~A" mime)
                mime)
              :default (format t "OPEN-DOCUMENT does not know how to handle a document of mime type : ~A.~%"
                               (magic-mime which)))
           ("application/x-bzip2" (bunzip2 which))
           ("application/x-tar" (untar which)))
         (format t "No file found at ~A.~%" which)))))

(defun bunzip2 (filename)
  (let* ((suffix (pathname-type filename))
         (unzipped-filename (subseq (namestring filename) 0 
                                    (- (length filename) (1+ (length suffix))))))
    (if (probe-file unzipped-filename)
      (format t "The destination file ~S already exists.~%" unzipped-filename)
      (progn
        (cl-bzip2:decompress (pathname filename) (pathname unzipped-filename))
        (format t "sucessful bunzip2 : ~A -> ~A~%" filename (file-namestring unzipped-filename))))))

(defun stat-formatted (filename)
  (when (probe-file filename)
    (let ((stat (sb-posix:stat filename)))
      (format nil "~A" (sb-posix:stat-size stat)))))

(defun untar (filename)
  (let ((directory (directory-namestring filename)))
    (with-child-process (process (format nil "/bin/tar xvf ~A -C ~A" filename directory) :stdout t)
      (print-table
       (iter (for line = (read-line (process-output process) nil nil))
             (while line)
             (collect (list line (stat-formatted (concatenate 'string directory line)))))))))
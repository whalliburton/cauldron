;; open.lisp

(in-package :documents)

(defun open-document (which &optional (error-on-unknown-p t))
  "Open for reading any readable file or unarchive any archive."
  (etypecase which
    (string
       (if (probe-file which)
         (string-case
             ((let ((mime (magic-mime which)))
                (warn "mime ~A" mime)
                mime)
              :default
              (when error-on-unknown-p
                (format t "OPEN-DOCUMENT does not know how to handle a document of mime type : ~A.~%"
                        (magic-mime which))))
           ("application/x-bzip2" (bunzip2 which))
           ("application/x-gzip" (gunzip which))
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
        (format t "sucessful bunzip2 : ~A -> ~A~%" filename (file-namestring unzipped-filename))
        (open-document unzipped-filename nil)))))

(defun stat-formatted (filename)
  (when (probe-file filename)
    (let ((stat (sb-posix:stat filename)))
      (format nil "~A" (sb-posix:stat-size stat)))))

(defun check-tar-for-single-root-directory (filename)
  (let ((directory (directory-namestring filename)))
    (iter (for line in (process-lines (format nil "/bin/tar tf ~A -C ~A" filename directory)))
          (for dir = (subseq line 0 (position #\/ line)))
          (for last-dir previous dir)
          (while line)
          (when (and last-dir (string/= dir last-dir))
            (return nil))
          (finally (return t)))))

(defun untar (filename)
  (if (check-tar-for-single-root-directory filename)
    (let ((directory (directory-namestring filename)))
      (print-table
       (iter (for line in (process-lines (format nil "/bin/tar xvf ~A -C ~A" filename directory)))
             (while line)
             (collect (list line (stat-formatted (concatenate 'string directory line)))))))
    (let ((dir (pathname-name filename)))
      (format t "~A would not expand into a single directory.~%Expanding instead into ~A~%"
              (file-namestring filename) dir)
      (let ((new-dir (concatenate 'string (directory-namestring filename) dir)))
        (if (probe-file new-dir)
          (format t "~A already exists, aborting.~%" dir)
          (progn
            (sb-posix:mkdir new-dir #o700)
            (print-table
             (iter (for line in (process-lines
                                 (format nil "/bin/tar xvf ~A -C ~A" filename new-dir)))
                   (while line)
                   (collect (list line (stat-formatted (concatenate 'string new-dir "/" line))))
                   ))))))))

(defun gunzip (filename)
  (let* ((directory (directory-namestring filename))
         (base-name (pathname-name filename))
         (extension (pathname-type filename))
         (uncompressed-filename
          (concatenate 'string directory base-name
                       (string-case (extension :default "")
                         ("tgz" ".tar")))))
    (assert (string/= (namestring filename) uncompressed-filename))
    (if (probe-file uncompressed-filename)
      (format t "aborting gunzip, ~S already exists~%" uncompressed-filename)
      (progn
        (gzip-stream:gunzip filename uncompressed-filename)
        (format t "sucessful gunzip : ~A -> ~A~%" filename (file-namestring uncompressed-filename))
        (open-document uncompressed-filename nil)))))
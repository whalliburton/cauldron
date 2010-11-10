;; bbdb.lisp

;; Access to emacs BBDB database file.

(in-package :communications)

(defparameter *bbdb-file* (in-home "/.bbdb"))

(defun read-vector-as-list (stream char)
  (read-delimited-list #\] stream t))

(defparameter *bbdb-readtable*
  (defreadtable bbdb-readtable
    (:merge :standard)
    (:macro-char #\[ 'read-vector-as-list)
    (:macro-char #\] (get-macro-character #\)))))

(defun read-bbdb-file ()
  (unless (probe-file *bbdb-file*)
    (error "No BBDB file exists at ~S." *bbdb-file*))
  (let ((*readtable* *bbdb-readtable*))
    (with-open-file (stream *bbdb-file*)
      (iter (for line = (read stream nil))
            (while line)
            (collect line)))))

(defun contacts ()
  "List all contacts."
  (print-table
   (iter (for (first-name last-name aka company phones addresses net-addresses notes)
              in (sort (read-bbdb-file) #'string< :key #'second))
         (collect (list (or first-name "") (or last-name "") (or (first net-addresses) ""))))
   :headings '("first name" "last name" "email")))

(defun find-contact (who)
  (iter (for (first-name last-name aka company phones addresses net-addresses notes)
             in (read-bbdb-file))
        (if (and net-addresses
                 (or (string-equal who first-name)
                     (string-equal who last-name)
                     (string-equal who (first net-addresses))))
          (return-from find-contact (first net-addresses))))
  (error "No contact called ~S found." who))
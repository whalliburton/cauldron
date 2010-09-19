;; pdf.lisp

(in-package :documents)

(defparameter *pdf-reader* "/opt/bin/acroread")

(defclass pdf (titled-document)
  ((author :initarg :author :reader author)
   (pages :initarg :pages :reader pages))
  (:metaclass persistent-class))

(defmethod print-object ((pdf pdf) stream)
  (with-slots (filename title author pages) pdf
    (print-unreadable-object (pdf stream :type t)
      (format stream "~A~@[ by ~A~]~@[ ~Ap~]" (or title filename) author pages))))

(defun pdf-info (filename)
  (iter (for line 
             in (split-sequence #\Newline
                                (run "/usr/bin/pdfinfo" filename)
                                :remove-empty-subseqs t))
        (for split = (position #\: line))
        (collect (list (subseq line 0 split)
                       (string-trim '(#\Space) (subseq line (1+ split)))))))

(defmethod import-document (filename (type (eql :|application/pdf|)))
  (let ((info-data (pdf-info filename)))
    (flet ((info (name) (cadr (assoc name info-data :test #'string-equal))))
      (let ((document
             (make-object 'pdf
                          :filename (file-namestring filename) 
                          :title (info 'title)
                          :author (info 'author)
                          :pages (info 'pages))))
        (blob-from-file document filename)
        document))))

(defmethod read-document ((document pdf))
  (run-program *pdf-reader* (list (namestring (blob-pathname document))) :wait nil))


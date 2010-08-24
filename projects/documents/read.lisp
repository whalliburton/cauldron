;; read.lisp

(in-package :documents)

(defparameter *pdf-reader* "/opt/bin/acroread")

(defun ensure-filename-is-pdf (filename)
  (unless (string= (magic-mime filename) "application/pdf")
    (error "~s is not a PDF file." filename))
  t)

(defun pdf-info (filename)
  (ensure-filename-is-pdf filename)
  (iter (for line 
             in (split-sequence #\Newline
                                (run "/usr/bin/pdfinfo" filename)
                                :remove-empty-subseqs t))
        (for split = (position #\: line))
        (collect (list (subseq line 0 split)
                       (string-trim '(#\Space) (subseq line (1+ split)))))))

(defclass document (blob)
  ((filename :initarg :filename :reader filename
             :index-type unique-index 
             :index-initargs (:test #'equalp)
             :index-reader document-with-filename)
   (title :initarg :title :reader title)
   (author :initarg :author :reader author)
   (pages :initarg :pages :reader pages))
  (:metaclass persistent-class))

(defmethod print-object ((document document) stream)
  (with-slots (filename title author pages) document
    (print-unreadable-object (document stream :type t)
      (format stream "~A~@[ by ~A~]~@[ ~Ap~]" (or title filename) author pages))))

(defun import-document (filename)
  (let ((info-data (pdf-info filename)))
    (flet ((info (name) (cadr (assoc name info-data :test #'string-equal))))
      (let ((document
             (make-object 'document 
                          :filename (pathname-name filename) 
                          :title (info 'title)
                          :author (info 'author)
                          :pages (info 'pages))))
        (blob-from-file document filename)
        document))))

(defgeneric read-document (document)
  (:documentation "Read a document.")
  (:method ((document document))
    (run-program *pdf-reader* (list (namestring (blob-pathname document))) :wait nil))
  (:method ((name string))
    (if-let (document (document-with-filename (file-namestring name)))
      (progn
        (warn "A document with filename ~a has already been imported, reading this." name)
        (read-document document))
      (if (and (probe-file name) (ensure-filename-is-pdf name))
        (read-document (import-document name))
        (error "Unknown document ~s." name))))
  (:method ((id integer))
    (let ((document (or (store-object-with-id id)
                        (error "No document with ID ~a found." id))))
      (unless (typep document 'document)
        (error "Object with id ~a is not a document." id))
      (read-document document))))

(defun list-documents ()
  "List all the readable documents."
  (print-table 
   (nconc (list (list "id" "filename" "author" "pages" "title")
                (list "==" "========" "======" "=====" "====="))
          (mapcar (lambda (doc) (list (store-object-id doc) (filename doc) 
                                      (or (author doc) "")
                                      (pages doc) (or (title doc) "")))
                  (sort (store-objects-with-class 'document)
                        #'string< :key #'filename)))))

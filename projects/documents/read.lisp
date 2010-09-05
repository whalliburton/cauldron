;; read.lisp

(in-package :documents)

(defparameter *pdf-reader* "/opt/bin/acroread")

(defclass base-document (blob)
  ((filename :initarg :filename :reader filename
             :index-type unique-index 
             :index-initargs (:test #'equalp)
             :index-reader document-with-filename))
  (:metaclass persistent-class))

(defmethod title ((obj base-document)) nil)
(defmethod author ((obj base-document)) nil)
(defmethod pages ((obj base-document)) nil)

(defmethod print-object ((base-document base-document) stream)
  (with-slots (filename) base-document
    (print-unreadable-object (base-document stream :type t)
      (format stream "~A" filename))))

(defclass titled-document (base-document)
  ((title :initform nil :initarg :title :reader title))
  (:metaclass persistent-class))

(defclass document (titled-document)
  ((author :initarg :author :reader author)
   (pages :initarg :pages :reader pages))
  (:metaclass persistent-class))

(defmethod print-object ((document document) stream)
  (with-slots (filename title author pages) document
    (print-unreadable-object (document stream :type t)
      (format stream "~A~@[ by ~A~]~@[ ~Ap~]" (or title filename) author pages))))

(defun pdf-info (filename)
  (iter (for line 
             in (split-sequence #\Newline
                                (run "/usr/bin/pdfinfo" filename)
                                :remove-empty-subseqs t))
        (for split = (position #\: line))
        (collect (list (subseq line 0 split)
                       (string-trim '(#\Space) (subseq line (1+ split)))))))

(defun import-pdf-document (filename)
  (let ((info-data (pdf-info filename)))
    (flet ((info (name) (cadr (assoc name info-data :test #'string-equal))))
      (let ((document
             (make-object 'document 
                          :filename (file-namestring filename) 
                          :title (info 'title)
                          :author (info 'author)
                          :pages (info 'pages))))
        (blob-from-file document filename)
        document))))

(defun import-text-document (filename)
  (let ((document (make-object 'base-document :filename (file-namestring filename))))
    (blob-from-file document filename)
    document))

(defun import-html-document (filename)
  (let ((document (make-object 'titled-document :filename (file-namestring filename))))
    (blob-from-file document filename)
    document))

(defparameter *html-viewer* :emacs)

(defgeneric read-document (document) 
  (:documentation "Read a document.")
  (:method ((document base-document))
    (view-in-emacs (filename document) (namestring (blob-pathname document))))
  (:method :after ((document base-document))
    (format t "reading : ~A~%" (filename document)))
  (:method ((document titled-document))
    (let ((link-name (create-blob-link (filename document) document)))
      (case *html-viewer*
        (:emacs (view-in-emacs-w3m link-name))
        (:web (view-in-web-browser link-name)))))
  (:method ((document document))
    (run-program *pdf-reader* (list (namestring (blob-pathname document))) :wait nil))
  (:method ((name string))
    (if-let (document (document-with-filename (file-namestring name)))
      (progn
        (warn "A document with filename ~a has already been imported, reading this." name)
        (read-document document))
      (progn
        (unless (probe-file name)
          (error "File not found: ~s" name))
        (string-case  ((magic-mime name))
          ("application/pdf" (read-document (import-pdf-document name)))
          ("text/plain" (read-document (import-text-document name)))
          ("text/x-lisp" (read-document (import-text-document name)))
          ("text/html" (read-document (import-html-document name)))))))
  (:method ((id integer))
    (let ((document (or (store-object-with-id id)
                        (error "No document with ID ~a found." id))))
      (unless (typep document 'base-document)
        (error "Object with id ~a is not a document." id))
      (read-document document))))

(defun list-documents ()
  "List all the readable documents."
  (print-table 
   (mapcar (lambda (doc) (list (store-object-id doc) 
                               (pathname-name (filename doc)) 
                               (pathname-type (filename doc))
                               (or (author doc) "")
                               (or (pages doc) "") 
                               (or (title doc) "")))
           (sort (store-objects-with-class 'base-document)
                 #'string< :key #'filename))
   :headings '("id" "filename" "type" "author" "pages" "title")))
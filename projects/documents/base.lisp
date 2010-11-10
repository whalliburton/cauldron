;; base.lisp

(in-package :documents)

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
      (format stream "~A ~A" (store-object-id base-document) filename))))

(defclass titled-document (base-document)
  ((title :initform nil :initarg :title :reader title))
  (:metaclass persistent-class))

(defun get-document (id)
  (let ((document (or (store-object-with-id id)
                      (error "No document with ID ~a found." id))))
    (unless (typep document 'base-document)
      (error "Object with id ~a is not a document." id))
    document))

(defun delete-document (id)
  "Remove a document from the database."
  (when-let (document (get-document id))
    (delete-object document)))

(defvar *inhibit-read-message* nil)

(defparameter *html-viewer* :emacs)

(defgeneric read-document (document)
  (:documentation "Read a document.")
  (:method ((document base-document))
    (if swank::*emacs-connection*
      (view-in-emacs (filename document) (namestring (blob-pathname document)))
      (write-string (slurp (blob-pathname document) 'character))))
  (:method :around ((document base-document))
    (let (*inhibit-read-message*)
      (call-next-method)
      (unless *inhibit-read-message*
        (format t "sucessfully read : ~A~%" (filename document)))))
  (:method ((document titled-document))
    (let ((link-name (create-blob-link (filename document) document)))
      (case *html-viewer*
        (:emacs (view-in-emacs-w3m link-name))
        (:web (view-in-web-browser link-name)))))
  (:method ((name string))
    (if-let (document (document-with-filename (file-namestring name)))
      (progn
        (warn "A document with filename ~a has already been imported, reading this." name)
        (read-document document))
      (progn
        (unless (probe-file name) (error "File not found: ~s" name))
        (read-document (import-document name (ksymb (magic-mime name)))))))
  (:method ((id integer)) (read-document (get-document id))))

(defun import-simple-document (filename type)
  (let ((document (make-object type :filename (file-namestring filename))))
    (blob-from-file document filename)
    document))

(defgeneric import-document (name type)
  (:method (name type) (error "Unknown file type : ~A~%" type))
  (:method (name (type (eql :|text/plain|))) (import-simple-document name 'base-document))
  (:method (name (type (eql :|text/x-lisp|))) (import-simple-document name 'base-document))
  (:method (name (type (eql :|text/html|))) (import-simple-document name 'titled-document)))

(defun list-documents (&optional (maximum-column-width 40))
  "List all the readable documents."
  (if-let ((base-documents (store-objects-with-class 'base-document)))
    (print-table
     (mapcar (lambda (doc) (list (store-object-id doc)
                                 (pathname-name (filename doc))
                                 (pathname-type (filename doc))
                                 (or (author doc) "")
                                 (or (pages doc) "")
                                 (or (title doc) "")))
             (sort base-documents #'string< :key #'filename))
     :headings '("id" "filename" "type" "author" "pages" "title")
     :max-column-width maximum-column-width :oversize-suffix "...")
    (format t "none~%")))

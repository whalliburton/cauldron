;; ideas.lisp

(in-package :databases)

(defclass idea (store-object)
  ((text :initform nil :initarg :text))
  (:metaclass persistent-class))

(defmethod print-object ((idea idea) stream)
  (print-unreadable-object (idea stream :type t)
    (write-string (slot-value idea 'text) stream)))

(defun add-idea (text)
  "Add an idea to the idea database."
  (make-object 'idea :text text))

(defun list-ideas ()
  "List all the ideas in the idea database."
  (mapc
   (lambda (el) (princ (slot-value el 'text)) (newline))
   (sort (store-objects-with-class 'idea) #'< 
         :key (lambda (el) (slot-value el 'bknr.datastore::last-change))))
  (values))

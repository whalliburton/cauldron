;; shtooka.lisp

(in-package :language)

(defun parse-shtooka-tags (data)
  (with-input-from-string (stream data)
    (iter 
      (for line = (read-line stream nil))
      (while line)
      (when (and (plusp (length line)) (char= #\[ (char line 0)))
        (collect 
            (list (subseq line 1 (1- (length line)))
                  (iter 
                    (for line = (read-line stream nil))
                    (while line)
                    (when (zerop (length line)) (return tags))
                    (when-let (pos (position #\= line))
                      (collect
                          (cons (intern (substitute #\- #\_ (subseq line 0 pos)) :keyword)
                                (subseq line (1+ pos) (1- (length line))))
                        into tags)))))))))

(defun list-shtooka-packets (&optional check-for-newer)
  (parse-shtooka-tags 
   (cached-http-request 
    "http://packs.shtooka.net/index.packs.txt" 
    :check-for-newer check-for-newer)))

(defun list-shtooka-packet-names (&optional check-for-newer)
  (mapcar #'first (list-shtooka-packets check-for-newer)))

(defun list-shtooka-languages (&optional check-for-newer)
  (sort
   (mapcar 'language-name-from-code
           (remove-duplicates
            (mapcar (lambda (el)
                      (string-case (el :default el)
                        ("cm" "zh")
                        ("po" "pt")
                        ("sp" "es")
                        ("wu" "zh")))
                    (mapcar (lambda (el) (cdr (assoc :pack-langs (second el))))
                            (list-shtooka-packets check-for-newer))) :test #'string=))
   #'string<))

(defun ensure-valid-shtooka-packet-name (name &optional check-for-newer)
  (unless (member name (list-shtooka-packet-names check-for-newer) :test #'string=)
    (error "No shtooka packet named ~S." name)))

(defun list-shtooka-packet-words (packet-name &optional check-for-newer)
  (ensure-valid-shtooka-packet-name packet-name)
  (parse-shtooka-tags
   (cached-http-request 
    (format nil "http://packs.shtooka.net/~a/ogg/index.tags.txt" packet-name) 
    :check-for-newer check-for-newer)))

(defun ensure-valid-shtooka-packet-word (packet-name filename &optional check-for-newer)
  (or (assoc filename (list-shtooka-packet-words packet-name check-for-newer) :test #'string=)
      (error "Shtooka packet ~s does not have a file named ~s." packet-name filename)))

(defclass shtooka-word (cached-http-request)
  ((text :initarg :text :reader text))
  (:metaclass persistent-class))

(defmethod print-object ((shtooka-word shtooka-word) stream)
  (print-unreadable-object (shtooka-word stream :type t)
    (format stream "~A ~A" (store-object-id shtooka-word) (text shtooka-word))))

(defun shtooka-packet-word (packet-name filename &optional check-for-newer)
  (ensure-valid-shtooka-packet-name packet-name check-for-newer)
  (cached-http-request
   (format nil "http://packs.shtooka.net/~a/ogg/~a" packet-name filename)
   :check-for-newer check-for-newer :return-object t
   :class 'shtooka-word
   :initargs (list :text 
                   (cdr (assoc :swac-text
                               (cadr (assoc filename (list-shtooka-packet-words packet-name) 
                                            :test #'string=)))))))

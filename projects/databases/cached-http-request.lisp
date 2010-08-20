;; cached-http-request.lisp

(in-package :databases)

(defclass cached-http-request (blob)
  ((uri :initarg :uri
        :reader uri
        :index-type unique-index 
        :index-initargs (:test #'equalp)
        :index-reader cached-http-request-with-uri
        :index-values all-cached-http-requests)
   (alias :initarg :alias
          :reader alias
          :index-type unique-index 
          :index-initargs (:test #'equalp)
          :index-reader cached-http-request-with-alias)
   (last-modified :initarg :last-modified :reader last-modified))
  (:metaclass persistent-class))

(defmethod print-object ((cached cached-http-request) stream)
  (with-slots (uri alias type) cached
    (print-unreadable-object (cached stream :type t)
      (format stream "~A ~A ~A~@[ [~A~]] " (store-object-id cached) type uri alias))))

(defun cached-http-request-body (cached)
  (let ((element-type (case (blob-type cached)
                        (:text 'character)
                        (t '(unsigned-byte 8)))))
    (with-open-blob (in cached :direction :input :element-type element-type)
      (slurp-stream in element-type))))

(defun uri-last-modified (uri)
  (multiple-value-bind (body code headers) (http-request uri :method :head)
    (declare (ignore body))
    (and (eql code 200)
         (cdr (assoc :last-modified headers)))))

(defun cached-http-request-newer-exists (cached)
  (not (equalp (last-modified cached) 
               (uri-last-modified (uri cached)))))

(deftransaction set-last-modified (cached last-modified)
  (setf (slot-value cached 'last-modified) last-modified))

(deftransaction set-blob-type (cached blob-type)
  (setf (slot-value cached 'type) blob-type))

(defun set-cached-http-request (uri &rest args &key return-object alias &allow-other-keys)
  (with-assoc (content-type last-modified location)
    (multiple-value-bind (body code headers) 
        (apply 'http-request uri :redirect nil
               (remove-keywords args :return-object :check-for-newer :alias))
      (cond
        ((eql code 200)
         (let* ((type (blob-type-from-content-type (:content-type headers)))
                (object (or (cached-http-request-with-uri uri)
                            (make-object 'cached-http-request :uri uri :type type :alias alias))))
           (set-last-modified object (:last-modified headers))
           (if (eq type :text)
             (blob-from-string object body)
             (blob-from-array object body))
           (when (eq type :unknown)
             (maybe-set-type-from-magic object))
           (if return-object object body)))
        ((eql code 302)
         (apply 'set-cached-http-request (:location headers) :alias uri args))))))

(defun maybe-set-type-from-magic (object)
  (awhen (string-case ((magic-mime (namestring (blob-pathname object))) 
                    :default nil )
           ("audio/x-wav" :wav))
    (set-blob-type object it)))

(defun blob-type-from-content-type (content-type)
  (cond 
    ((string-starts-with content-type "text/plain") :text)
    ((string-starts-with content-type "application/ogg") :ogg)
    (t :unknown)))

(defun cached-http-request (uri &rest args &key check-for-newer return-object
                            &allow-other-keys)
  (let ((cached (or (cached-http-request-with-uri uri)
                    (cached-http-request-with-alias uri))))
    (if cached
      (if check-for-newer
        (if (cached-http-request-newer-exists cached)
          (apply 'set-cached-http-request uri args)
          (if return-object cached (cached-http-request-body cached)))
        (if return-object cached (cached-http-request-body cached)))
      (apply 'set-cached-http-request uri args))))

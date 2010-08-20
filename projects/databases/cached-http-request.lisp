;; cached-http-request.lisp

(in-package :databases)

(defclass cached-http-request (blob)
  ((uri :initarg :uri
        :reader uri
        :index-type unique-index 
        :index-initargs (:test #'equalp)
        :index-reader cached-http-request-with-uri
        :index-values all-cached-http-requests)
   (last-modified :initarg :last-modified :reader last-modified))
  (:metaclass persistent-class))

(defmethod print-object ((cached cached-http-request) stream)
  (with-slots (uri type) cached
    (print-unreadable-object (cached stream :type t)
      (format stream "~A ~A" type uri))))

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

(deftransaction set-cached-http-request (uri &optional return-object)
  (with-assoc (content-type last-modified)
    (multiple-value-bind (body code headers) (http-request uri)
      (when (eql code 200)
        (let* ((type (blob-type-from-content-type (:content-type headers)))
               (object (or (cached-http-request-with-uri uri)
                           (make-object 'cached-http-request :uri uri :type type))))
          (setf (slot-value object 'last-modified) (:last-modified headers))
          (if (eq type :text)
            (blob-from-string object body)
            (blob-from-array object body))
          (if return-object object body))))))

(defun blob-type-from-content-type (content-type)
  (cond 
    ((string-starts-with content-type "text/plain") :text)
    ((string-starts-with content-type "application/ogg") :ogg)
    (t :unknown)))

(defun cached-http-request (uri &optional check-for-newer return-object)
  (let ((cached (cached-http-request-with-uri uri)))
    (if cached
      (if check-for-newer
        (if (cached-http-request-newer-exists cached)
          (set-cached-http-request uri return-object)
          (if return-object cached (cached-http-request-body cached)))
        (if return-object cached (cached-http-request-body cached)))
      (set-cached-http-request uri return-object))))

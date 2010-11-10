;; freesound.lisp

(in-package :music)

(defparameter *freesound-username* "whalliburton")
(defparameter *freesound-password* "freesound")
(defparameter *freesound-cookies* (make-instance 'drakma:cookie-jar))

(defun freesound-login (&key (username *freesound-username*)
                             (password *freesound-password*))
  (http-request
   "http://www.freesound.org/forum/login.php"
   :method :post
   :parameters `(("username" . ,username)
                 ("password" . ,password)
                 ("redirect" . "../index.php")
                 ("login" . "login")
                 ("autologin" . "0"))
   :cookie-jar *freesound-cookies*))

(defun ensure-freesound-login ()
  (unless (cookie-jar-cookies *freesound-cookies*)
    (freesound-login)))

(define-xml-request raw-freesound-search-text
    (&key search (start 0) (search-descriptions 1) (search-tags 1)
          (search-filenames 0) (search-usernames 0) (duration-min 0)
          (duration-max 20) (order 0) (limit 100))
  "http://www.freesound.org/searchTextXML.php"
  :camel-caps t
  :cookie-jar *freesound-cookies*
  :raw t :remove-whitespace t :initial ensure-freesound-login)

(defun freesound-search (query &rest args &key (limit 10) ids-only &allow-other-keys)
  (let ((ids (iter (for rtn in (raw-freesound-search-text :search query :limit limit))
                   (when (and (consp rtn) (stringp (car rtn)) (string= (car rtn) "sample"))
                     (collect (parse-integer
                               (second (assoc "id" (second rtn) :test #'string=))))))))
    (if ids-only ids
      (print-table (mapcar (lambda (el) (apply 'freesound-view el args)) ids)))))

;; ORDER_DEFAULT = 0;
;; ORDER_DOWNLOADS_DESC = 1;
;; ORDER_DOWNLOADS_ASC = 2;
;; ORDER_USERNAME_DESC = 3;
;; ORDER_USERNAME_ASC = 4;
;; ORDER_DATE_DESC = 5;
;; ORDER_DATE_ASC = 6;
;; ORDER_DURATION_DESC = 7;
;; ORDER_DURATION_ASC = 8;
;; ORDER_FILEFORMAT_DESC = 9;
;; ORDER_FILEFORMAT_ASC = 10;

(define-xml-request raw-freesound-view-single (&key id)
  "http://www.freesound.org/samplesViewSingleXML.php"
  :cookie-jar *freesound-cookies* :raw t :remove-whitespace t
  :initial ensure-freesound-login :force-binary t)

(defun freesound-view (id &key (with-id t) date channels (duration t)
                       filesize (tags t) description (extension t) &allow-other-keys)
  (let* ((raw (raw-freesound-view-single :id id))
         (props (delete-if 'null (cddr (assoc "sample" (cdr raw) :test #'string=)))))
    (flet ((extract (name &optional (els props))
             (delete-if 'null (cdr (assoc name els :test #'string=)))))
      `(,@(when with-id (list id))
          ,@(when date (list (first (extract "date"))))
          ,@(when channels (list (first (extract "channels"))))
          ,@(when extension (list (first (extract "extension"))))
          ,@(when duration (list (format nil "~,2F" (parse-float (first (extract "duration"))))))
          ,@(when filesize (list (first (extract "filesize"))))
          ,@(when tags (list (mapcar #'third (extract "tags"))))
          ,@(when description
              (list (first (extract "text" (cddr (first (extract "descriptions")))))))))))

(defun freesound-download (id)
  (ensure-freesound-login)
  (cached-http-request
   (format nil "http://www.freesound.org/samplesDownload.php?id=~a" id)
   :cookie-jar *freesound-cookies* :force-binary t :return-object t))

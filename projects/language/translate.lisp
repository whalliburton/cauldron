;; translate.lisp

(in-package :language)

(defparameter *google-local-api-key*
  "ABQIAAAAHYi0aYuHYG8eTFIStkTpVxSRnkGKXhTILQQvmRvP010BcEsVlhSNijEtesznO0mmICc6xTqnyw1xIw")

(defun url-encode (string)
  (with-output-to-string (stream)
    (iter (for char in-vector string)
          (if (alphanumericp char)
            (write-char char stream)
            (format stream "%~2,'0x" (char-code char))))))

(defun google-translate (query langpair &key (key *google-local-api-key*))
  (decode-json-from-string
   (http-request
    (format nil "http://ajax.googleapis.com/ajax/services/language/translate?&v=1.0~
                    &key=~a&langpair=~a" key (url-encode langpair))
    :method :post :external-format-out :utf-8 :parameters `(("q" . ,query)))))

(defun translate (query &optional (langpair "en|ru"))
  "Translate QUERY from one language to another."
  (cdr (assoc :translated-text
              (cdr (assoc :response-data
                          (google-translate query langpair))))))



;; weather.lisp

(in-package :databases)

;; might be better to user google's weather api

(defun weather (&optional (location "80905"))
  "Show the weather for the current location."
  (let ((xml
         (http-request
          (format nil "http://api.wunderground.com/auto/wui/geo/ForecastXML/index.xml?query=~A"
                  location))))
    (print-heading (format nil "The weather for ~A." location))
    (with-input-from-string (stream xml)
      (iter (for line in-stream stream using #'read-line)
            (let ((base (string-trim '(#\space #\tab) line)))
              (when (string-starts-with base "<title>")
                (print-heading (subseq base 7 (- (length base) 8)) *standard-output* 1))
              (when (string-starts-with base "<fcttext>")
                (princ (subseq base 9 (- (length base) 10)))
                (newline 2)))))))
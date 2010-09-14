;; regex.lisp

(in-package :utilities)

;;; from Doug Hoyte's Let Over Lambda

(defun segment-reader (stream ch n)
  (when (> n 0)
    (let ((chars))
      (do ((curr (read-char stream)
             (read-char stream)))
          ((char= ch curr))
        (push curr chars))
      (cons (coerce (nreverse chars) 'string)
            (segment-reader stream ch (- n 1))))))

(defmacro subst-mode-ppcre-lambda-form (args)
  (once-only (args)
    (with-gensyms (str)
      ``(lambda (,',str)
          (cl-ppcre:regex-replace-all
           ,(car ,args)
           ,',str
           ,(cadr ,args))))))

(defmacro match-mode-ppcre-lambda-form (args mods fn)
  (once-only (args mods)
    (with-gensyms (str)
      ``(lambda (,',str)
          (,,fn
           ,(if (zerop (length ,mods))
              (car ,args)
              (format nil "(?~a)~a" ,mods (car ,args)))
           ,',str)))))

(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((or (char= mode-char #\m) (char= mode-char #\c))
       (match-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        1)
        (coerce (iter (for c = (read-char stream))
                      (while (alpha-char-p c))
                      (collect c)
                      (finally (unread-char c stream)))
                'string)
        (if (char= mode-char #\m) 'cl-ppcre:scan 'cl-ppcre:scan-to-strings)))
      ((char= mode-char #\s)
       (subst-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        2)))
      (t (error "Unknown #~~ mode character")))))

(set-dispatch-macro-character #\# #\~ #'|#~-reader|)

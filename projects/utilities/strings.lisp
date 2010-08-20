;; strings.lisp

(in-package :utilities)

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((mismatch (mismatch prefix string :test test)))
    (or (not mismatch) (= mismatch (length prefix)))))

(defun string-ends-with (string suffix &key (test #'char=))
  "Returns true if STRING ends with PREFIX."
  (let ((mm 0))
    (iter (for end1 from (1- (length string)) downto 0)
          (for end2 from (1- (length suffix)) downto 0)
          (while (funcall test (aref string end1) (aref suffix end2)))
          (incf mm))
    (= mm (length suffix))))

(defun string-contains-p (string string-to-find)
  (search string-to-find string :test #'string=))

(defun process-string (string commands)
  "A simple language for splitting and subseqing strings."  
  (iter (with working = string) 
        (for command in commands)
        (let ((function (ecase (first command)
                          (:split 'process-split)
                          (:trim 'process-trim)
                          (:up-to 'process-up-to))))
          (setf working 
                (if (consp working)
                  (mapcar (lambda (el) (process-string el (list command))) working)
                  (apply function working (cdr command)))))
        (finally (return working))))

(defun process-split (string delimiter)
  (split-sequence delimiter string :remove-empty-subseqs t))

(defun process-trim (string &rest characters)
  (string-trim characters string))

(defun process-up-to (string character)
  (subseq string 0 (or (position character string)
                       (error "Character ~s was not found in ~s." character string))))

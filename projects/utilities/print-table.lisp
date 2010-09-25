;; print-table.lisp

(in-package :utilities)

(defun max-min-row-length (rows)
  (iter (for row in rows)
        (for len = (length row))
        (maximizing len into max-len)
        (minimizing len into min-len)
        (finally (return (values max-len min-len)))))

(defun row-extend (rows len)
  (iter (for row in rows)
        (for row-len = (length row))
        (if (/= len row-len)
          (collect (append row (make-list (- len row-len) :initial-element "")))
          (collect row))))

;(row-extend '((1 2 3) (1)) 5)

(defun print-table (rows &key indent (spacing 2) (stream t) max-column-width
                         min-column-width oversize-suffix
                         title (indent-title t) right-justified
                         headings)
  (when title
    (format stream
            (if (and indent (< 0 indent) indent-title)
              (format nil "~~~AT~~A~~%" indent)
              "~A~%")
            title)
    (when (and indent (< 0 indent) indent-title)
      (dotimes (x indent) (write-char #\Space stream)))
    (dotimes (x (length title)) (write-char #\- stream))
    (newline stream))
  (when headings 
    (setf rows (nconc
                (list headings
                      (iter (for heading in headings)
                            (collect (make-string (length heading) :initial-element #\=))))
                rows)))
  (when rows
    (multiple-value-bind (max-row min-row) (max-min-row-length rows)
      (let* ((maxs (make-array max-row :initial-element 0))
             (row-strings
              (iter 
                (for row in (if (/= min-row max-row) 
                              (row-extend rows max-row) 
                              rows))
                (collect 
                    (iter 
                      (for el in row)
                      (collect
                          (let ((str (princ-to-string el)))
                            (cond
                              ((and max-column-width 
                                    (> (length str) max-column-width))
                               (let ((base (subseq str 0 max-column-width)))
                                 (if oversize-suffix 
                                   (concatenate 'string 
                                                (subseq base 0 (- (length base)
                                                                  (length oversize-suffix)))
                                                oversize-suffix)
                                   base)))
                              ((and min-column-width
                                    (< (length str) min-column-width))
                               (pad-string str min-column-width))
                              (t str)))))))))
        (iter (for row in row-strings)
              (iter (for el in row)
                    (for x upfrom 0)
                    (setf (aref maxs x) (max (aref maxs x) (length el)))))
        (let ((format-string
               (with-output-to-string (str)
                 (when (and indent (< 0 indent)) (princ (format nil "~~~AT" indent) str))
                 (iter (for size in-vector maxs)
                       (for x upfrom 1)
                       (format str "~~~A~Aa" (+ size spacing) (if right-justified "@" "")))
                 (princ "~%" str))))
          (iter (for row in row-strings)
                (apply #'format stream format-string row)))))))

(defun blank-table-line () (list (list)))
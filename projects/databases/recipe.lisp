;; recipe.lisp

(in-package :databases)

(defun recipe-query (query &optional ingredients page)
  (multiple-value-bind (rtn code)
      (http-request 
       (format nil "http://www.recipepuppy.com/api/?~@[i=~{~a~^,~}&~]q=~a~@[&p=~a~]"
               ingredients query page))
    (when (eql code 200)
      (decode-json-from-string rtn))))

(defun recipe (query &optional ingredients page)
  (let ((recipes (recipe-query query ingredients page)))
    (when recipes
      (iter (for recipe in (cdr (assoc :results recipes)))
            (format t "~a~%~a~%~a~%~%"
                    (cdr (assoc :title recipe))
                    (cdr (assoc :ingredients recipe))
                    (cdr (assoc :href recipe)))))))
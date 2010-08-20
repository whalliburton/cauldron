;; help.lisp

(in-package :spaceship)

(defun subsystem-help-text (subsystem)
  (when-let (symbol (find-symbol "*HELP-TEXT*" subsystem))
    (and (boundp symbol) (symbol-value symbol))))

(defun all-spaceship-subsystems ()
  (iter (for package in (list-all-packages))
        (when-let (help (subsystem-help-text package))
          (collect package))))

(defun help (&optional subsystem)
  "Display help for the spaceship and all it's subsystems. Any package
with a *help-text* external symbol is considered a subsystem. All
external functions in these packages should have docstrings."
  (if subsystem
    (if-let (help-text (subsystem-help-text subsystem))
      (progn
        (format t "Help for ~(~A~).~%~%  ~A~%~%" subsystem help-text)
        (print-table 
         (iter (for sym in-package subsystem external-only t)
               (when (fboundp sym)
                 (when-let (doc (documentation sym 'function))
                   (collect (list (string-downcase sym) doc)))))
         :indent 2)))
    (progn
      (format t "Help is available on the following sybsystems:~%~%")
      (print-table
       (iter (for system in (all-spaceship-subsystems))
             (collect
                 (list (concatenate 'string (string-downcase (package-name system)) ":")
                       (subsystem-help-text system))))
       :indent 2)
      (newline))))

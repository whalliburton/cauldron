;; help.lisp

(in-package :spaceship)

(defun subsystem-help-text (subsystem)
  (multiple-value-bind (symbol accessable) (find-symbol "*HELP-TEXT*" subsystem)
    (when (eq accessable :external)
      (symbol-value symbol))))

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
                 (collect (list (string-downcase sym)
                                (documentation sym 'function)))))
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

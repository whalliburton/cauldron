;; help.lisp

(in-package :spaceship)

(defun subsystem-help-text (subsystem)
  (when-let (symbol (find-symbol "*HELP-TEXT*" subsystem))
    (and (boundp symbol) (symbol-value symbol))))

(defun all-spaceship-subsystems ()
  (sort 
   (iter (for package in (list-all-packages))
         (when-let (help (subsystem-help-text package))
           (collect package)))
   #'string< :key #'package-name))

(defun help (&optional subsystem)
  "Display help for the spaceship and all it's subsystems. Any package
with a *help-text* external symbol is considered a subsystem. All
external functions in these packages should have docstrings."
  (if subsystem
    (if (eq subsystem t)
      (progn (mapc #'help (mapcar #'package-name (all-spaceship-subsystems))) nil)
      (if-let (help-text (subsystem-help-text subsystem))
        (progn
          (newline)
          (print-heading (format nil "Help for the ~(~A~) subsystem" subsystem))
          (format t "  ~A~%~%" help-text)
          (print-table 
           (iter (for sym in-package subsystem external-only t)
                 (when (fboundp sym)
                   (when-let (doc (documentation sym 'function))
                     (collect (list (string-downcase sym) doc)))))
           :indent 2 :spacing 3)
          (newline))))
    (progn
      (newline)
      (print-heading "Help is available on the following sybsystems")
      (print-table
       (iter (for system in (all-spaceship-subsystems))
             (collect
                 (list (string-downcase (package-name system))
                       (subsystem-help-text system))))
       :indent 2 :spacing 3)
      (newline)
      (format t "  Call HELP with a subsystem name for help on a particular subsystem,~%")
      (format t "  or T for help on all subsystems.~%")
      (newline))))

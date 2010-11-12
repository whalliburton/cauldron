;; help.lisp

(in-package :spaceship)

(defun subsystem-help-text (subsystem)
  (when-let (symbol (find-symbol "*HELP-TEXT*" subsystem))
    (and (boundp symbol) (symbol-value symbol))))

(defun all-subsystems ()
  (sort
   (iter (for package in (list-all-packages))
         (when-let (help (subsystem-help-text package))
           (collect package)))
   #'string< :key #'package-name))

(defun subsystem-functions (subsystem)
  (sort
   (iter (for sym in-package subsystem external-only t)
         (when (fboundp sym)
           (when-let (doc (documentation sym 'function))
             (collect (list sym doc))
             (when-let (alias (get sym :alias))
               (collect (list alias doc))))))
   'string< :key 'car))

(defun all-functions ()
  (iter (for subsystem in (all-subsystems))
        (nconcing (subsystem-functions subsystem))))


;; Any package with a *help-text* external symbol is considered a subsystem.
;; All external functions in these packages should have docstrings.

(defun help (&optional subsystem)
  "Display help for the spaceship and all it's subsystems."
  (let ((subsystem (and subsystem (string-upcase subsystem))))
    (when subsystem
      (cond
        ((string= subsystem "ALL") (setf subsystem :all))
        (t (if-let (package (find-package subsystem))
             (setf subsystem (package-name package))
             (error "No subsystem called ~A exists." subsystem)))))
    (if subsystem
      (if (eq subsystem :all)
        (progn (mapc #'help (mapcar #'package-name (all-subsystems))) nil)
        (if-let (help-text (subsystem-help-text subsystem))
          (progn
            (newline)
            (print-heading (format nil "Help for the ~(~A~) subsystem" subsystem))
            (format t "  ~A~%~%" help-text)
            (print-table
             (sort
              (iter (for sym in-package subsystem external-only t)
                    (for alias = (get sym :alias))
                    (when (fboundp sym)
                      (when-let (doc (documentation sym 'function))
                        (collect (list (string-downcase sym)
                                       (or (and alias (string-downcase alias)) "")
                                       doc)))))
              'string< :key 'car)
             :indent 2 :spacing 3)
            ;; the odd space is because bash's echo will strip a lone newline
            (format t " ~%"))))
      (progn
        (newline)
        (print-heading "Help is available on the following subsystems")
        (print-table
         (iter (for system in (all-subsystems))
               (collect
                   (list (string-downcase (package-name system))
                         (subsystem-help-text system))))
         :indent 2 :spacing 3)
        (newline)
        (format t "  Call HELP with a subsystem name for help on a particular subsystem,~%")
        (format t "  or ALL for help on all subsystems.~% ~%")))))

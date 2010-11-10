;; ids.lisp

(in-package :communications)

(defun list-ssh-ids ()
  (mapcar #L(split-sequence #\space %) (process-lines "/usr/bin/ssh-add -l")))

(defun ids ()
  "List the currently active network ids."
  (print-table (mapcar #L(list (first %) (second %) (third %)
                               (string-trim '(#\( #\)) (fourth %)))
                        (list-ssh-ids))
               :headings '("size" "fingerprint" "filename" "type")))
;; frequency.lisp

(in-package :language)

(defun load-word-frequency-list ()
  (iter (for line in-file "/lisp/projects/language/russian/5000-frequency.txt"             
             using #'read-line)
        (collect (cddr (split-sequence #\space (string-trim '(#\return) line))))))

(defun find-word-in-wordlist (word wordlist)
  (iter (for el in wordlist)
        (when (or (string-equal word (car el))
                  (string-starts-with (car el) word))
          (collect el))))

(defun match-frequency-to-wordlist ()
  (let ((wordlist (list-shtooka-packet-words 
                   '("rus-balm-voc" "rus-balm-voc-sakhno" "rus-nonfree"))))
    (iter (for word in (load-word-frequency-list))
          (collect (append word (find-word-in-wordlist (car word) wordlist))))))

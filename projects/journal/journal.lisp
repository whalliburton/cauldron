;; journal.lisp

(in-package :journal)

(defparameter *journal-directory* (in-home  "/life/journal/"))

(defun date-to-journal-filename (date)
  (format-timestring nil date :format '((:year 4) #\- (:month 2) #\- (:day 2))))

(defun possibly-create-journal-file (date)
  (ensure-directories-exist *journal-directory*)
  (let ((filename (concatenate 'string *journal-directory*
                               (date-to-journal-filename date))))
    (values filename
            (when (null (probe-file filename))
              (with-open-file (str filename :direction :output)
                (format-timestring str date :format
                                   '(:long-month  #\space :day #\, #\space (:year 4)))
                (newline 2 str))
              t))))

(defun journal ()
  "Fire up an emacs buffer editing the journal entry for the current day."
  (open-in-emacs (possibly-create-journal-file (today))))

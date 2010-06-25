;; Copyright (C) 2008 Shawn Betts
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

;; Commentary:
;;
;; Help and introspection commands
;;
;; Code:

(in-package #:stumpwm)

(export '())

(defun columnize (list columns &key col-aligns (pad 1) (char #\Space) (align :left))
  ;; only somewhat nasty
  (let* ((rows (ceiling (length list) columns))
         (data (loop for i from 0 below (length list) by rows
                     collect (subseq list i (min (+ i rows) (length list)))))
         (max (mapcar (lambda (col)
                        (reduce 'max col :key 'length :initial-value 0))
                      data))
         (padstr (make-string pad :initial-element char)))
    (apply 'mapcar 'concat
           ;; normalize width
           (loop
            for i in data
            for j in max
            for c from 0
            collect (loop
                     for k from 0 below rows
                     for s = (or (nth k i) "")
                     for len = (make-string (- j (length s))
                                            :initial-element char)
                     collect (ecase (or (nth c col-aligns) align)
                               (:left (format nil "~a~a~a" (if (= c 0) "" padstr) s len))
                               (:right (format nil "~a~a~a" (if (= c 0) "" padstr) len s))))))))

(defun display-bindings-for-keymaps (key-seq &rest keymaps)
  (let* ((screen (current-screen))
         (data (mapcan (lambda (map)
                         (mapcar (lambda (b) (format nil "^5*~5a^n ~a" (print-key (binding-key b)) (binding-command b))) (kmap-bindings map)))
                       keymaps))
         (cols (ceiling (1+ (length data))
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "Prefix: ~a~%~{~a~^~%~}"
                        (print-key-seq key-seq)
                        (columnize data cols))))

(defcommand commands () ()
"List all available commands."
  (let* ((screen (current-screen))
         (data (all-commands))
         (cols (ceiling (length data)
                        (truncate (- (head-height (current-head)) (* 2 (screen-msg-border-width screen)))
                                  (font-height (screen-font screen))))))
    (message-no-timeout "~{~a~^~%~}"
                        (columnize data cols))))

(defcommand describe-key (keys) ((:key-seq "Describe Key: "))
"Either interactively type the key sequence or supply it as text. This
command prints the command bound to the specified key sequence."
  (let ((cmd (loop for map in (top-maps)
                   for cmd = (lookup-key-sequence map keys)
                   when cmd return cmd)))
    (if cmd
        (message "~{~a~^ ~} is bound to \"~a\"." (mapcar 'print-key keys)  cmd)
        (message "~{~a~^ ~} is not bound." (mapcar 'print-key keys)))))

(defcommand describe-variable (var) ((:variable "Describe Variable: "))
"Print the online help associated with the specified variable."
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe var s))))

(defcommand describe-function (fn) ((:function "Describe Function: "))
"Print the online help associated with the specified function."
  (message-no-timeout "~a"
                      (with-output-to-string (s)
                        (describe fn s))))

(defcommand describe-command (com) ((:command "Describe Command: "))
  "Print the online help associated with the specified command."
  (let* ((deref (dereference-command-symbol com))
         (struct (get-command-structure com nil)))
    (cond ((null struct)
           (message "Error: Command \"~a\" not found." com))
          ((eq deref struct)
           (message-no-timeout "Command \"~a\":~%~a" (command-name struct)
                               (documentation (command-name struct) 'function)))
          (t
           (message-no-timeout "\"~a\" is an alias for the command \"~a\":~%~a" (command-alias-from deref) (command-name struct)
                               (documentation (command-name struct) 'function))))))

(defcommand where-is (cmd) ((:rest "Where is command: "))
"Print the key sequences bound to the specified command."
(let ((bindings (loop for map in (top-maps) append (search-kmap cmd map))))
  (if bindings
      (message-no-timeout "\"~a\" is on ~{~a~^, ~}"
                      cmd
                      (mapcar 'print-key-seq bindings))
      (message-no-timeout "Command \"~a\" is not currently bound"
                      cmd))))

(defcommand modifiers () ()
  "List the modifiers stumpwm recognizes and what MOD-X it thinks they're on."
  (message "~@{~5@a: ~{~(~a~)~^ ~}~%~}"
           "Meta" (modifiers-meta *modifiers*)
           "Alt" (modifiers-alt *modifiers*)
           "Super" (modifiers-super *modifiers*)
           "Hyper" (modifiers-hyper *modifiers*)
           "AltGr" (modifiers-altgr *modifiers*)))

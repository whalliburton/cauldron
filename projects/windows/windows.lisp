;; windows.lisp

(in-package :windows)

(defun list-windows ()
  "List all the desktop windows."
  (print-table
   (iter (for window in
              (sort (stumpwm::screen-windows (current-screen))
                    #'string< :key #'stumpwm::window-class))
         (collect
             (list
              (stumpwm::window-number window)
              (stumpwm::window-class window)
              (prin1-with-ellipses-to-string
               (stumpwm::window-title window) 80 #'princ-to-string))))
   :headings '("" "class" "title")))

(defun find-window (number)
  (or (find number (stumpwm::screen-windows (current-screen))
            :test #'= :key 'stumpwm::window-number)
      (error "No window with number ~S found." number)))

(defun destroy-window (number)
  "Destroy the window with number NUMBER."
  (let ((window(find-window number) ))
    (stumpwm::destroy-window window)
    (format t "Destroyed window ~S." (stumpwm::window-title window))))

;; This seems to only works from slime when you jiggle the mouse
;; afterwards, but it works fine from the shell.
(defun show-window (number)
  "Show and focus the window with number NUMBER."
  (stumpwm::focus-window (find-window number)))

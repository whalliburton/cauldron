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
              (stumpwm::window-title window))))
   :headings '("" "class" "title")))

(defun destroy-window (number)
  "Destroy the window with number NUMBER."
  (stumpwm::destroy-window
   (or (find number (stumpwm::screen-windows (current-screen))
             :test #'= :key 'stumpwm::window-number)
       (error "No window with number ~S found." number))))


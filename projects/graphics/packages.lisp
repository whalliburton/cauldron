;; packages.lisp

(defpackage graphics
  (:use common-lisp cl-cairo2 cl-colors cffi)
  (:import-from sb-ext quit list-all-timers make-timer schedule-timer unschedule-timer)
  (:export window create-window close-window fill-with-color set-color 2PI
           updating-window))



  

	   
	   

	   
	

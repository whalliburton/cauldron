;; build-manual.lisp

(in-package :manual)

(defun build-manual ()
  (sb-texinfo:generate-includes "/lisp/projects/manual/include/"
                                (list :spaceship
                                      :communications
                                      :databases
                                      :hardware
                                      :journal
                                      :language
                                      :music
                                      :network
                                      :web
                                      :windows))
  (sb-posix:chdir "/lisp/projects/manual")
  (sb-ext:run-program "/usr/bin/makeinfo"
                      '("--html" "--no-split" "--css-include=style.css" "spaceship.texinfo")))

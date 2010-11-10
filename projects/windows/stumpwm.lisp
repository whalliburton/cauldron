;; stumpwm.lisp

(in-package :stumpwm)

(setf *startup-message* nil)

(defun default-swank-port ()
  (+ 20000
     (parse-integer
      (or (sb-ext:posix-getenv "DISPLAY")
          (error "No DISPLAY set.")) :start 1 :end 2)))

(defcommand swank () ()
  (setf stumpwm:*top-level-error-action* :break)
  (loop for try-port = (default-swank-port) then (1+ try-port)
        while (handler-case
                  (progn
                    (swank:create-server :port try-port
                                         :style swank:*communication-style*
                                         :dont-close t :coding-system "utf-8-unix")
                    (message "Swank started on port ~a." try-port)
                    nil)
                (sb-bsd-sockets:address-in-use-error ()
                  (warn "SWANK port ~a taken, trying the next." try-port)
                  t))))

(defcommand emacs-spaceship () ()
  (run-or-raise "emacs --eval \"(connect-to-spaceship)\"" '(:class "Emacs")))

;; --enable-ipv6 is to workaround chromium failing to open localhost
;; when no resolver is present
(defcommand chromium () ()
  (run-or-raise "chromium --enable-ipv6" '(:title "Chromium")))

(defcommand firefox () ()
  (run-or-raise "firefox" '(:title "Firefox")))

(defcommand conkeror () ()
  (run-or-raise "conkeror" '(:title "Conkeror")))

(defcommand iptraf () ()
  (run-or-raise "exec urxvt -e /lisp/scripts/paused-iptraf" '(:title "iptraf")))

(defcommand tmux () ()
  (run-or-raise "exec urxvt -tn rxvt-unicode256 -e tmux attach" '(:title "tmux")))

(defcommand htop () ()
  (run-or-raise "exec urxvt -e htop" '(:title "htop")))

(defcommand xkill () ()
  (run-shell-command "xkill"))

(defcommand iotop () ()
  (run-or-raise "exec urxvt -e iotop -o" '(:title "iotop")))

(defcommand anki () ()
  (run-or-raise "anki" '(:title "anki")))

(defun goto-win (win)
  (let* ((group (window-group win))
         (frame (window-frame win))
         (old-frame (tile-group-current-frame group)))
    (frame-raise-window group frame win)
    (focus-all win)
    (unless (eq frame old-frame)
      (show-frame-indicator group))))

(defcommand xpdf () ()
  (let ((hits (append
               (find-matching-windows '(:title "Xpdf") t nil)
               (find-matching-windows '(:instance "acroread") t nil)
               (find-matching-windows '(:instance "gv") t nil))))
    (if (car hits)
      (goto-win (car hits))
      (message "No running Xpdf."))))

(defcommand start-terminal (&optional arg) ((:string "suffix: "))
  (run-or-raise  (format nil "exec urxvt -name urxvt~@[-~A~]" arg) `(:instance ,arg)))

(progn
  (setf *top-map* nil *root-map* nil *help-map* nil *group-top-map* nil
        *tile-group-top-map* nil *tile-group-root-map* nil *groups-map* nil)

  (fill-keymap
   *top-map*
   (kbd "s-x")       '*root-map*
   (kbd "s-h")       *help-map*
   (kbd "F1")        "emacs-spaceship"
   (kbd "F2")        "tmux"
   (kbd "F3")        "chromium"
   (kbd "F4")        "conkeror"
   (kbd "F5")        "start-terminal 0"
   (kbd "F6")        "start-terminal 1"
   (kbd "F7")        "start-terminal 2"
   (kbd "F8")        "start-terminal 3"
   (kbd "F9")        "xpdf"
   (kbd "s-a")       "anki"
   (kbd "s-h")       "htop"
   (kbd "s-i")       "iotop"
   (kbd "s-Up")      "move-focus up"
   (kbd "s-Down")    "move-focus down"
   (kbd "s-Left")    "move-focus left"
   (kbd "s-Right")   "move-focus right"
   (kbd "s-M-Up")    "move-window up"
   (kbd "s-M-Down")  "move-window down"
   (kbd "s-M-Left")  "move-window left"
   (kbd "s-M-Right") "move-window right"
   (kbd "s-'")       "windowlist"
   (kbd "s-w")       "show-window-properties"
   (kbd "s-SPC")     "pull-hidden-next"
   (kbd "s-;")       "colon"
   (kbd "s-:")       "eval"
   (kbd "s-3")       "vsplit"
   (kbd "s-2")       "hsplit"
   (kbd "s-1")       "only"
   (kbd "s-f")       "firefox"
   (kbd "s-n")       "iptraf")

  (fill-keymap
   *root-map*
   (kbd "m")   "lastmsg"
   (kbd "b")   "banish"
   (kbd "k")   "xkill"
   (kbd "!")   "exec"
   (kbd "s-g") "abort"
   (kbd "G")   "vgroups"
   (kbd "g")   '*groups-map*
   (kbd "h")   '*help-map*)

  (fill-keymap
   *group-top-map*
   (kbd "s-x") '*group-root-map*)

  (fill-keymap *group-root-map*
               (kbd "s-u") "next-urgent"
               (kbd "w")   "windows"
               (kbd "s-w") "windows"
               (kbd "k")   "delete"
               (kbd "s-k") "delete"
               (kbd "K")   "kill"
               (kbd "'")   "select"
               (kbd "s-N") "number"
               (kbd "#")   "mark"
               (kbd "F11") "fullscreen"
               (kbd "A")   "title"
               (kbd "i")   "info")

  (fill-keymap
   *tile-group-top-map*
   (kbd "s-x") '*tile-group-root-map*)

  (fill-keymap
   *tile-group-root-map*

   (kbd "n")       "pull-hidden-next"
   (kbd "s-n")     "pull-hidden-next"
   (kbd "M-n")     "next"
   (kbd "s-M-n")   "next-in-frame"
   (kbd "SPC")     "pull-hidden-next"
   (kbd "s-SPC")   "pull-hidden-next"
   (kbd "p")       "pull-hidden-previous"
   (kbd "s-p")     "pull-hidden-previous"
   (kbd "M-p")     "prev"
   (kbd "s-M-p")   "prev-in-frame"
   (kbd "W")       "place-existing-windows"
   (kbd "s-x")     "pull-hidden-other"
   (kbd "M-t")     "other-in-frame"
   (kbd "s-0")     "pull 0"
   (kbd "s-1")     "pull 1"
   (kbd "s-2")     "pull 2"
   (kbd "s-3")     "pull 3"
   (kbd "s-4")     "pull 4"
   (kbd "s-5")     "pull 5"
   (kbd "s-6")     "pull 6"
   (kbd "s-7")     "pull 7"
   (kbd "s-8")     "pull 8"
   (kbd "s-9")     "pull 9"
   (kbd "R")       "remove"
   (kbd "r")       "iresize"
   (kbd "o")       "fnext"
   (kbd "TAB")     "fnext"
   (kbd "M-TAB")   "fother"
   (kbd "f")       "fselect"
   (kbd "F")       "curframe"
   (kbd "-")       "fclear"
   (kbd "+")       "balance-frames"
   (kbd "l")       "redisplay"
   (kbd "C-l")     "redisplay")

  (fill-keymap
   *groups-map*
   (kbd "g")     "groups"
   (kbd "c")     "gnew"
   (kbd "n")     "gnext"
   (kbd "s-n")   "gnext"
   (kbd "SPC")   "gnext"
   (kbd "s-SPC") "gnext"
   (kbd "N")     "gnext-with-window"
   (kbd "p")     "gprev"
   (kbd "s-p")   "gprev"
   (kbd "P")     "gprev-with-window"
   (kbd "o")     "gother"
   (kbd "'")     "gselect"
   (kbd "\"")    "grouplist"
   (kbd "m")     "gmove"
   (kbd "M")     "gmove-marked"
   (kbd "k")     "gkill"
   (kbd "A")     "grename"
   (kbd "r")     "grename")

  (fill-keymap
   *help-map*
   (kbd "v") "describe-variable"
   (kbd "f") "describe-function"
   (kbd "k") "describe-key"
   (kbd "c") "describe-command"
   (kbd "w") "where-is"))

(defun load-rc-file ()
  (run-shell-command "xsetroot -cursor_name left_ptr")

;; Load swank.
  (load "/lisp/site-lisp/slime/swank-loader.lisp")
  (swank-loader:init)
  (swank)

  (banish)
  (emacs-spaceship)

  (run-shell-command "unclutter -idle 1 -jitter 2 -root")

  (sync-keys))

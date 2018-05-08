(require 'exwm-goto)

;;;; Quickly run an application or switch to it if it is already running
(defun akirak/exwm-goto-browser ()
  (interactive)
  (exwm-goto "firefox" :class "Firefox"))

;;;; Taking screenshots

(defcustom akirak/screenshot-file-name
  "~/Dropbox/Screenshots/scrot-%F-%T.png"
  "File name template for screenshots. Used by scrot.")

(defun akirak/screenshot (&optional delay)
  (interactive "P")
  (apply #'start-process "scrot" nil
         "scrot"
         `(,@(if (numberp delay)
                 (list "-d" (int-to-string delay))
               (list "-s"))
           ,(expand-file-name akirak/screenshot-file-name))))

(defun akirak/kazam ()
  (interactive)
  (start-process "kazam" nil "kazam"))

;;;; Alternate window manager

(defcustom akirak/alternate-window-manager
  "startxfce4"
  "Window manager (or X application) run by `akirak/alternate-window-manager'.")

(defcustom akirak/xephyr-window-size "1024x768"
  "The size of a Xephyr window.")

(defcustom akirak/x-alternate-display ":1"
  "The X display used by Xephyr.")

(defun akirak/alternate-window-manager ()
  (interactive)
  (async-shell-command
   (concat "Xephyr -br -ac -noreset -screen "
           akirak/xephyr-window-size
           " "
           akirak/x-alternate-display
           "&\n"
           "DISPLAY="
           akirak/x-alternate-display
           " "
           akirak/alternate-window-manager
           "\n"
           "kill %1")
   "*window-manager*"))

(provide 'akirak-x-apps)

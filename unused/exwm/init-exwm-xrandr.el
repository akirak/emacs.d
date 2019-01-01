(require 'exwm-randr)

(setq exwm-workspace-number 2)

(setq exwm-randr-workspace-output-plist
      (list 0 "HDMI1"
            1 "HDMI2"))

(defun akirak/exwm-configure-screens ()
  (start-process-shell-command
   "xrandr" nil
   "xrandr --output HDMI2 --right-of HDMI1 --auto"))
(add-hook 'exwm-randr-screen-change-hook #'akirak/exwm-configure-screens)

(exwm-randr-enable)

(provide 'init-exwm-xrandr)

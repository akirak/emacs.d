(require 'init-exwm)
(require 'init-exwm-ui)
(require 'init-exwm-hooks)
(require 'init-exwm-tray)
(require 'init-exwm-xrandr)
(require 'init-lemonbar)
(require 'init-exwm-bindings)
(require 'akirak-x-apps)

(akirak/define-frame-workflow "web"
  :layout '(progn
             (start-process-shell-command "firefox" nil "firefox")
             (delete-other-windows)))

(server-start)

;; Load my personal configuration
(when (file-exists-p "~/ops/init.el")
  (load-file "~/ops/init.el"))

(provide 'init-exwm-config)

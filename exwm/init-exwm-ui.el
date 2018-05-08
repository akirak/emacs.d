;;; Basic settings for EXWM
(setq mouse-autoselect-window t
      use-dialog-box nil)

;; Save the screen space by disabling menu-bar
(menu-bar-mode -1)

;;; window-divider
(setq window-divider-default-right-width 2)
(setq window-divider-default-bottom-width 2)
(window-divider-mode 1)

;;; Floating window borders
(setq exwm-floating-border-width 3)
(setq exwm-floating-border-color "orange")

(provide 'init-exwm-ui)

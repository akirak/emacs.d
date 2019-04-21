(use-package exwm
  :commands (exwm-enable))

;; Mouse follows focus
(use-package exwm-mff
  :straight (exwm-mff :host github :repo "ieure/exwm-mff"))

(use-package exwm-edit)

(provide 'setup-exwm)

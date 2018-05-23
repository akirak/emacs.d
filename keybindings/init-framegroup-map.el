(define-prefix-command 'akirak/framegroup-map)

(general-def :keymaps 'akirak/framegroup-map
  "R" 'fg-rename-frame
  "/" 'fg-switch-to-frame)

(provide 'init-framegroup-map)

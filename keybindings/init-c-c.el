(general-def
  "C-c b" 'magit-branch-popup
  "C-c i" 'insert-register
  "C-c j" '(katawa-ivy :keymap text-mode-map)
  "C-c J" '(katawa-ivy-fix :keymap text-mode-map)
  "C-c l" 'org-store-link
  "C-c p" 'yankpad-map
  "C-c y" 'yankpad-insert
  "C-c v" 'ivy-push-view
  "C-c V" 'ivy-switch-view)

(provide 'init-c-c)

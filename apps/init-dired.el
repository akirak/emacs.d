(setq-default dired-dwim-target t)

;;;; Packages for enhancing dired
(use-package dired-hide-dotfiles
  :commands (dired-hide-dotfiles-mode))

(use-package dired-filter
  :hook
  (dired . dired-filter-mode))

;;;; Additional keybindings in dired-mode
(general-def dired-mode-map
  "zh" #'dired-hide-dotfiles-mode)

(provide 'init-dired)

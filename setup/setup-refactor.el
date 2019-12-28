(use-package emr
  :config
  (akirak/bind-replace :keymaps 'prog-mode-map
    "SPC" #'emr-show-refactor-menu))

(provide 'setup-refactor)

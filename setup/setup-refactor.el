(use-package emr
  :config
  (akirak/bind-generic :keymaps 'prog-mode-map
    "r SPC" #'emr-show-refactor-menu))

(provide 'setup-refactor)

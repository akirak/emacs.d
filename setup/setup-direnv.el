(use-package direnv
  :config
  (direnv-mode 1))

(use-package projectile-direnv
  :hook
  (projectile-mode . projectile-direnv-export-variables))

(provide 'setup-direnv)

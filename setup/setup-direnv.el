(use-package direnv
  :config
  (direnv-mode 1))

(use-package projectile-direnv
  :disabled t
  :hook
  (projectile-mode . projectile-direnv-export-variables))

(provide 'setup-direnv)

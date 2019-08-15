(use-package direnv
  :config
  (direnv-mode 1)
  :custom
  (direnv-always-show-summary nil))

(use-package projectile-direnv
  :disabled t
  :hook
  (projectile-mode . projectile-direnv-export-variables))

(provide 'setup-direnv)

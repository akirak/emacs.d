(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  :hook
  (prog-mode . turn-on-smartparens-strict-mode))

(provide 'setup-smartparens)

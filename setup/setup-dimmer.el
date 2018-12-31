(use-package dimmer
  :init
  (dimmer-mode 1)
  :custom
  (dimmer-exclusion-regexp "\\(\\*Help\\*\\|\\*helm\\)"))

(provide 'setup-dimmer)

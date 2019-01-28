(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  :general
  (:keymaps 'smartparens-mode-map
            [remap down-list] #'sp-down-sexp)
  :hook
  (prog-mode . turn-on-smartparens-strict-mode))

(provide 'setup-smartparens)

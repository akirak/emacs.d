(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  :general
  (:keymaps 'smartparens-mode-map
            [remap forward-sexp] #'sp-forward-sexp
            [remap backward-sexp] #'sp-backward-sexp
            [remap backward-up-list] #'sp-backward-up-sexp)
  :hook
  (prog-mode . turn-on-smartparens-strict-mode))

(provide 'setup-smartparens)

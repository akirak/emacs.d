(use-package smartparens
  :config
  (require 'smartparens-config)
  (defun akirak/setup-smartparens-mode ()
    (interactive)
    (electric-pair-local-mode -1)
    (unless (bound-and-true-p polymode-mode)
      (when (derived-mode-p 'prog-mode 'json-mode 'sgml-mode)
        (turn-on-smartparens-strict-mode))))
  :hook
  (after-change-major-mode . akirak/setup-smartparens-mode)
  (minibuffer-setup . turn-on-smartparens-strict-mode))

(akirak/bind-generic :keymaps 'smartparens-mode-map
  "p" '(nil :wk "smartparens")
  "pe" #'sp-change-enclosing
  "pr" #'sp-raise-sexp
  "pC" 'sp-convolute-sexp
  "pf" #'sp-forward-barf-sexp
  "pb" #'sp-backward-barf-sexp
  "pu" 'sp-unwrap-sexp)

(provide 'setup-smartparens)

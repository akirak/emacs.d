(use-package smartparens
  :config
  (require 'smartparens-config)
  (defun akirak/setup-smartparens-mode ()
    (unless (bound-and-true-p polymode-mode)
      (when (derived-mode-p 'prog-mode)
        (turn-on-smartparens-strict-mode)
        (show-smartparens-mode 1))))
  :hook
  (after-change-major-mode . akirak/setup-smartparens-mode)
  (minibuffer-setup . smartparens-mode))

(akirak/bind-user :keymaps 'smartparens-mode-map
  "e" #'sp-change-enclosing)

(general-def :keymaps 'smartparens-mode-map :prefix "C-S-s"
  "r" #'sp-raise-sexp
  "C" 'sp-convolute-sexp
  "b" '(nil :wk "barf")
  "bf" #'sp-forward-barf-sexp
  "bb" #'sp-backward-barf-sexp
  "/" 'sp-unwrap-sexp)

(general-def :keymaps 'smartparens-mode-map
  "M-(" #'sp-wrap-round)

(provide 'setup-smartparens)

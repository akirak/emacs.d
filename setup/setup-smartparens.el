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

(akirak/bind-generic :keymaps 'smartparens-mode-map
  "bb" 'sp-backward-slurp-command
  "bf" 'sp-forward-slurp-command
  "C" 'sp-convolute-sexp
  "js" 'sp-join-sexp
  "ks" '(nil :wk "splice")
  "ksa" 'sp-splice-sexp-killing-around
  "ksb" 'sp-splice-sexp-killing-backward
  "ksf" 'sp-splice-sexp-killing-forward
  "rw" 'sp-rewrap-sexp
  "sb" 'sp-backward-slurp-sexp
  "sf" 'sp-forward-slurp-sexp
  "sj" 'sp-split-sexp
  "us" 'sp-splice-sexp
  "uw" 'sp-unwrap-sexp)

(general-def :keymaps 'smartparens-mode-map
  "M-(" #'sp-wrap-round)

(provide 'setup-smartparens)

(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  :hook
  (prog-mode . turn-on-smartparens-strict-mode))

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

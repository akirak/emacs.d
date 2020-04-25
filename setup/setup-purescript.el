;; Largely based on https://github.com/purcell/emacs.d/blob/master/lisp/init-purescript.el

(defconst akirak/spago-compile-command-list
  '("spago build"
    "spago test"
    "spago install"
    "spago run"
    "spago bundle-app"
    "spago bundle-module"
    "spago docs"))

(use-package purescript-mode
  :mode "\\.purs\\'"
  :config
  (akirak/bind-mode :keymaps 'purescript-mode-map
    "<tab>" #'purescript-indent-cycle
    "g" '(:wk "navigate")
    "gi" #'purescript-navigate-imports
    "," (defrepeater 'purescript-move-nested-left)
    "." (defrepeater 'purescript-move-nested-right))
  :hook
  (purescript-mode . turn-on-purescript-indentation)
  (purescript-mode . turn-on-purescript-decl-scan))

(use-package psc-ide
  :after purescript-mode
  :config
  (defun akirak/setup-psc-ide-mode ()
    (flycheck-mode t)
    ;; (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (turn-on-purescript-indentation))
  (akirak/bind-mode :keymaps 'psc-ide-mode-map
    "s" '(:wk "server")
    "ss" #'psc-ide-server-start
    "sq" #'psc-ide-server-quit
    "l" '(:wk "load")
    "la" #'psc-ide-load-all
    "ll" #'psc-ide-load-module
    "f" '(:wk "fix")
    "fa" #'psc-ide-flycheck-insert-suggestion
    "fc" #'psc-ide-add-clause
    "fs" #'psc-ide-case-split
    "fi" #'psc-ide-add-import
    "b" #'psc-ide-rebuild)
  :hook
  (purescript-mode . psc-ide-mode)
  (psc-ide-mode . akirak/setup-psc-ide-mode)
  :custom
  (psc-ide-add-import-on-completion t))

(use-package psci
  :hook
  (purescript-mode . inferior-psci-mode))

(provide 'setup-purescript)

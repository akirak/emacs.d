;; Largely based on https://github.com/purcell/emacs.d/blob/master/lisp/init-purescript.el

(use-package purescript-mode
  :mode "\\.purs\\'")

(use-package psc-ide
  :after purescript-mode
  :config
  (defun akirak/setup-psc-ide-mode ()
    (flycheck-mode t)
    ;; (setq-local flycheck-check-syntax-automatically '(save mode-enabled))
    (turn-on-purescript-indentation))
  :hook
  (purescript-mode . psc-ide-mode)
  (psc-ide-mode . akirak/setup-psc-ide-mode)
  :custom
  (psc-ide-add-import-on-completion t))

(use-package psci
  :hook
  (purescript-mode . inferior-psci-mode))

(provide 'setup-purescript)

;; Largely based on https://github.com/purcell/emacs.d/blob/master/lisp/init-purescript.el

(use-package purescript-mode
  :config
  (defun akirak/setup-purescript-mode ()
    (turn-on-purescript-indentation)
    (add-hook 'before-save-hook 'purescript-sort-imports nil t))
  :hook
  (purescript-mode . akirak/setup-purescript-mode))

(use-package psc-ide
  :after purescript-mode
  :config
  (defun akirak/setup-psc-ide-mode ()
    (flycheck-mode 1)
    (setq-local flycheck-check-syntax-automatically '(save mode-enabled)))
  :hook
  (purescript-mode . psc-ide-mode)
  (psc-ide-mode . akirak/setup-psc-ide-mode))

(use-package psci
  :hook
  (purescript-mode . inferior-psci-mode))

(provide 'setup-purescript)

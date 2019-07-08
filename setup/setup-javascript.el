(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :interpreter "node")

(use-package js2-imenu-extras
  :straight js2-mode
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :disabled t
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode 1)
    (tide-hl-identifier-mode 1)
    (add-hook 'before-save-hook 'tide-format-before-save t t)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode 1))
  :hook
  (typescript-mode . (setup-tide-mode)))

(provide 'setup-javascript)

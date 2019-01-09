(use-package lsp-mode
  :commands lsp)

(use-package lsp-imenu
  :straight lsp-mode
  :after lsp-mode
  :hook
  (lsp-after-open . lsp-enable-imenu))

(use-package company-lsp
  :after (lsp-mode company)
  :commands company-lsp
  :init
  (add-hook 'company-backends 'company-lsp))

;; TODO: Bind keys
(use-package lsp-ui :after lsp
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-ui-doc-mode-hook
            (lambda () (when lsp-ui-doc-mode (eldoc-mode -1))))
  (add-hook 'lsp-ui-mode-hook 'lsp-ui-doc-enable)
  :hook
  (lsp-mode . lsp-ui-mode))

(provide 'setup-lsp)

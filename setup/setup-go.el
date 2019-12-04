(use-package go-mode)

;; Use lsp-mode

(use-package go-rename
  ;; Prefer LSP.
  :disabled t
  :after go-mode)

(use-package go-eldoc
  ;; Prefer LSP.
  :disabled t
  :after go-mode
  :hook
  (go-mode . go-eldoc-setup))

(use-package company-go
  ;; Prefer LSP.
  :disabled t
  :after (company go-mode)
  :company go-mode)

(provide 'setup-go)

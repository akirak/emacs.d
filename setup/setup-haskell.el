(use-package intero
  :disabled t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package haskell-mode
  :disabled t
  :mode ("\\.hs\\'" . haskell-mode))

(use-package dante
  :disabled t
  :commands (dante-mode)
  :init
  (add-hook 'haskell-mode-hook 'dante-mode))

;; To use lsp-haskell, you also need to install haskell-ide-server.
;; This can take a lot of time, so I will disable lsp-haskell for now.
(use-package lsp-haskell
  :disabled t
  :straight (lsp-haskell :host github :repo "emacs-lsp/lsp-haskell")
  :hook
  (haskell-mode . lsp-haskell-enable))

(provide 'setup-haskell)

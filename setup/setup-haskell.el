(use-package intero
  :disabled t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode))

(use-package dante
  :commands (dante-mode)
  :init
  (add-hook 'haskell-mode-hook 'dante-mode))

(use-package lsp-haskell
  ;; To use lsp-haskell, you also need to install haskell-ide-server.
  ;; This can take a lot of time, so install it manually if you want
  ;; to write Haskell code.
  :if (executable-find "haskell-ide-server")
  :straight (lsp-haskell :host github :repo "emacs-lsp/lsp-haskell")
  :hook
  (haskell-mode . lsp-haskell-enable))

(use-package haskell-interactive-mode
  :disabled t
  :commands interactive-haskell-mode)

(provide 'setup-haskell)

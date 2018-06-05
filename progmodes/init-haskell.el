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

(provide 'init-haskell)

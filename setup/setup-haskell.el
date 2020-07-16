(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (defun akirak/setup-haskell-mode ()
    "Turn on either lsp-mode or dante-mode."
    (interactive)
    (unless (derived-mode-p 'haskell-mode)
      (user-error "Not haskell-mode"))
    (cond
     ((and (executable-find "ghcide")
           (require 'lsp-haskell nil t))
      (lsp))
     ((require 'dante nil t)
      (dante-mode 1))))
  :hook
  (haskell-mode . akirak/setup-haskell-mode))

(use-package dante
  :commands (dante-mode)
  (defun akirak/setup-dante ()
    (flycheck-mode t))
  :hook
  (dante-mode . akirak/setup-dante))

(use-package haskell-interactive-mode
  ;; I was unable to set up interactive-haskell-mode for snack.
  ;; Maybe I'll work on it later.
  :disabled t
  :commands interactive-haskell-mode
  :hook
  (haskell-mode . interactive-haskell-mode)
  :custom
  (haskell-process-suggest-remove-import-lines t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t))

(provide 'setup-haskell)

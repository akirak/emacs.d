(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (defun akirak/setup-haskell-mode ()
    (interactive)
    (unless (derived-mode-p 'haskell-mode)
      (user-error "Not haskell-mode"))
    (cond
     ((and (executable-find "haskell-ide-server")
           (featurep 'lsp-haskell))
      (lsp-haskell-enable))
     ((featurep 'dante)
      (dante-mode 1))))
  :hook
  (haskell-mode . akirak/setup-haskell-mode))

(use-package dante
  :commands (dante-mode))

(use-package lsp-haskell
  ;; To use lsp-haskell, you also need to install haskell-ide-server.
  ;; This can take a lot of time, so install it manually if you want
  ;; to write Haskell code.
  :straight (lsp-haskell :host github :repo "emacs-lsp/lsp-haskell")
  :config
  (defun akirak/maybe-turn-off-dante-mode ()
    (when (and (bound-and-true-p lsp-mode)
               (bound-and-true-p dante-mode)
               (derived-mode-p 'haskell-mode))
      (dante-mode -1)))
  :hook
  (lsp-mode . akirak/maybe-turn-off-dante-mode)
  :custom
  (lsp-haskell-process-path-hie "ghcide")
  (lsp-haskell-process-args-hie nil))

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

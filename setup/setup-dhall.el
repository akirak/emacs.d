(use-package dhall-mode
  :mode "\\.dhall\\'"
  :config
  (add-hook 'dhall-mode-hook
            (defun akirak/setup-dhall-lsp ()
              (when (require 'lsp-mode nil t)
                (lsp)
                (remove-hook 'lsp-eldoc-hook 'lsp-hover t)
                (setq-local lsp-eldoc-render-all nil)
                (setq-local lsp-eldoc-enable-hover nil)))))

(provide 'setup-dhall)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (require 'lsp-clients)
  (defun akirak/setup-lsp ()
    (company-mode t)
    (eldoc-mode t)
    (flycheck-mode t))
  ;; Update direnv to detect locally installed lsp servers
  (advice-add 'lsp :before
              (lambda (&rest _args) (direnv-update-environment)))
  :hook
  (lsp-mode . akirak/setup-lsp)
  ((web-mode
    vue-html-mode
    css-mode
    go-mode)
   . lsp-deferred)
  :custom
  (lsp-eldoc-render-all t))

(use-package lsp-server
  :straight (:host github :repo "akirak/lsp-server.el")
  :commands (lsp-server-install))

(use-package lsp-ui
  :disabled t
  :commands lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  (lsp-ui-mode . lsp-ui-sideline-mode)
  :custom
  (lsp-ui-doc-delay 0.8)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-alignment 'window)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-position (quote at-point)))

(use-package lsp-ivy
  :straight (:host github :repo "emacs-lsp/lsp-ivy")
  :config
  (akirak/bind-search :keymaps 'lsp-mode-map
    "s" #'lsp-ivy-workspace-symbol
    "M-s" #'lsp-ivy-global-workspace-symbol))

(use-package lsp-treemacs
  :disabled t
  :after lsp-mode)

(use-package dap-mode
  :after lsp-mode
  :hook
  (lsp-mode . dap-mode)
  (dap-mode . dap-ui-mode))

;;;; Additional LSP client packages which are not part of lsp-mode
(use-package lsp-dockerfile
  :straight (lsp-dockerfile :host github :repo "emacs-lsp/lsp-dockerfile")
  :after dockerfile-mode
  :hook
  (dockerfile-mode . lsp-dockerfile-enable)
  ;; :ensure-system-package
  ;; (docker-langserver . "sudo npm i -g dockerfile-language-server-nodejs")
  )

(use-package lsp-python
  :after python-mode
  :hook
  (python-mode . lsp-python-enable)
  ;; TODO: Install the executable using nix or something
  ;; :ensure-system-package python-language-server
  )

(use-package lsp-haskell
  :after haskell-mode
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

(provide 'setup-lsp)

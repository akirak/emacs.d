(use-package dockerfile-mode
  :init
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package lsp-dockerfile
  :straight (lsp-dockerfile :host github :repo "emacs-lsp/lsp-dockerfile")
  :after dockerfile-mode
  :hook
  (dockerfile-mode . lsp-dockerfile-enable)
  :ensure-system-package
  (docker-langserver . "sudo npm i -g dockerfile-language-server-nodejs"))

(provide 'init-dockerfile)

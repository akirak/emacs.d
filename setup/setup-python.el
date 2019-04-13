(use-package lsp-python :after lsp
  :hook
  (python-mode . lsp-python-enable)
  ;; TODO: Install the executable using nix or something
  ;; :ensure-system-package python-language-server
  )

(provide 'setup-python)

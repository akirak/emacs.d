(use-package lsp-python :after lsp
  :hook
  (python-mode . lsp-python-enable)
  ;; FIXME: Which package on Nix?
  ;; :ensure-system-package python-language-server
  )

(provide 'setup-python)

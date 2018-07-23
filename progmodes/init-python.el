;;;; Add support for lsp-mode
(require 'init-lsp)

(use-package lsp-python
  :hook
  (python-mode . lsp-python-enable)
  :ensure-system-package python-language-server)

(provide 'init-python)

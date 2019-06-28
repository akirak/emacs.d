(use-package vue-mode
  ;; Turn on vue-mode manually only when necessary
  ;; :mode "\\.vue\\'"
  )

(use-package lsp-vetur
  :straight lsp-mode
  :after vue-mode
  :custom
  (lsp-vetur-server "lsp"))

(provide 'setup-vue)

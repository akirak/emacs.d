(use-package js
  :straight (:type built-in))

;; Prefer js-mode for LSP support
(use-package js2-mode
  :disabled t
  :mode (("\\.js\\'" . js2-mode))
  :interpreter "node")

(use-package js2-imenu-extras
  :straight js2-mode
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :hook
  ((js-mode typescript-mode) . tide-setup)
  (tide-mode . tide-hl-identifier-mode)
  (tide-mode . flycheck-mode))

(provide 'setup-javascript)

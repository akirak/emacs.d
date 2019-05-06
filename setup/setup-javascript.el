(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode))
  :interpreter "node")

(use-package js2-imenu-extras
  :straight js2-mode
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package typescript-mode
  :mode "\\.ts\\'")

(provide 'setup-javascript)

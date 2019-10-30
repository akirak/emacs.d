(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2 "Fallback set in use-package"))

;; Prefer js-mode for LSP support
(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)))

(use-package js2-imenu-extras
  :straight js2-mode
  :after js2-mode
  :hook (js2-mode . js2-imenu-extras-mode))

(use-package js-comint
  :config
  (akirak/bind-mode-repl :keymaps '(js-mode-map typescript-mode-map)
    "" #'js-comint-repl))

(use-package add-node-modules-path
  :after js
  :hook
  ((js-mode js2-mode typescript-mode) . add-node-modules-path))

(use-package skewer-mode
  :hook
  (js2-mode . skewer-mode))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package tide
  :init
  (defun akirak/maybe-tide-setup ()
    (when (and (buffer-file-name)
               (not (derived-mode-p 'json-mode)))
      (tide-setup)))
  :hook
  ((js-mode typescript-mode) . akirak/maybe-tide-setup)
  (tide-mode . tide-hl-identifier-mode)
  (tide-mode . eldoc-mode)
  (tide-mode . flycheck-mode))

(provide 'setup-javascript)

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

(use-package js-comint)

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

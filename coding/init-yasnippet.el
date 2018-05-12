(use-package yasnippet
  :after company
  :diminish 'yas-minor-mode
  :config
  (yas-global-mode 1)
  (with-eval-after-load 'company
    (add-hook 'company-backends 'company-yasnippet t)))

(provide 'init-yasnippet)

(use-package yasnippet
  :after company
  :diminish 'yas-minor-mode
  :config
  (yas-global-mode 1)
  (add-to-list 'company-backends 'company-yasnippet t))

(provide 'init-yasnippet)

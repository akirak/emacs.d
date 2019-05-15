(use-package yasnippet
  ;; :diminish 'yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet
  :load-path "contrib/yasnippet-snippets"
  :straight (yasnippet-snippets :type built-in))

(use-package auto-yasnippet
  :commands (aya-create aya-expand))

(use-package ivy-yasnippet
  :commands (ivy-yasnippet))

(provide 'setup-yasnippet)

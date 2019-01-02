(use-package yasnippet-snippets
  :straight (yasnippet-snippets :host github :repo "akirak/yasnippet-snippets"
                                :files ("*.el" "snippets" ".nosearch"))
  :config
  ;; This should be set before loading yasnippet package
  (setq yas-snippet-dirs `(,(expand-file-name "straight/repos/yasnippet-snippets/snippets"
                                              user-emacs-directory))))

(use-package yasnippet
  :after (yasnippet-snippets)
  :diminish 'yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package auto-yasnippet
  :commands (aya-create aya-expand))

(use-package ivy-yasnippet
  :commands (ivy-yasnippet))

(provide 'setup-yasnippet)

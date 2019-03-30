(use-package perfect-margin
  :straight (perfect-margin :host github :repo "mpwang/perfect-margin")
  :config
  (perfect-margin-mode 1)
  :custom
  (perfect-margin-visible-width 92)
  (perfect-margin-ignore-regexps `("^minibuf" "^[*]"
                                   "^ \\*LV\\*"
                                   "^ \\*which-key\\*")))

(provide 'setup-perfect-margin)

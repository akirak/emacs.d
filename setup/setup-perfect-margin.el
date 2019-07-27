(use-package perfect-margin
  :straight
  ;; (perfect-margin :host github :repo "mpwang/perfect-margin")
  (perfect-margin :host github :repo "akirak/perfect-margin"
                  :branch "split-window")
  :config
  (perfect-margin-mode 1)
  :custom
  (perfect-margin-visible-width 92)
  (perfect-margin-ignore-modes '(exwm-mode
                                 doc-view-mode
                                 nov-mode
                                 vterm-mode))
  (perfect-margin-ignore-regexps `("^minibuf" "^[*]"
                                   "^ \\*LV\\*"
                                   "^ \\*which-key\\*")))

(provide 'setup-perfect-margin)

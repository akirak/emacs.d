(use-package perfect-margin
  :straight (:host github :repo "akirak/perfect-margin")
  :config
  (perfect-margin-mode 1)
  :custom
  (perfect-margin-visible-width 92)
  (perfect-margin-ignore-modes '(exwm-mode
                                 doc-view-mode
                                 pdf-view-mode
                                 nov-mode
                                 vterm-mode
                                 html-mode
                                 ;; For log files
                                 fundamental-mode))
  (perfect-margin-ignore-regexps `("^minibuf" "^[*]"
                                   "^ \\*LV\\*"
                                   "^ \\*which-key\\*")))

(provide 'setup-perfect-margin)

(use-package emr
  :general
  (:keymaps 'prog-mode-map
            "<M-return>" #'emr-show-refactor-menu
            "<M-RET>" #'emr-show-refactor-menu))

(provide 'setup-refactor)

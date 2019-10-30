(use-package emr
  :general
  (:keymaps 'prog-mode-map
            "<M-return>" #'emr-show-refactor-menu
            "<M-RET>" #'emr-show-refactor-menu)
  (:keymaps 'lispy-mode-map :package 'lispy
            [remap lispy-meta-return] 'emr-show-refactor-menu))

(provide 'setup-refactor)

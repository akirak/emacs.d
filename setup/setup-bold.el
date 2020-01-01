(use-package bold
  :straight (bold :host github :repo "akirak/bold.el")
  :config
  (require 'bold-setup)
  (bold-setup-hooks)
  :general
  (:keymaps 'bold-mode-map :package 'bold
            "C-c f" #'bold-fix-at-point
            "C-c r" #'bold-find-references-at-point
            "C-c q" #'bold-format-region
            [help ?.] #'bold-documentation-at-point
            "M-." #'bold-jump-to-definition
            "M-," #'bold-jump-back
            "C-. r ." #'bold-refactor
            "C-. r s" #'bold-rename-symbol
            "C-. r f" #'bold-rename-file
            "C-. f f" #'bold-format-region
            "C-. f o" #'bold-organize-imports
            "C-. ?" #'bold-error-at-point
            "C-. i b" #'bold-insert-block-comment)
  :custom
  (bold-documentation-at-point-fallback #'helpful-at-point))

(use-package bold-avy
  :straight bold
  :custom
  (bold-avy-prefix-key "C-;"))

(provide 'setup-bold)

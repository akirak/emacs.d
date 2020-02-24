;; Mode-agnostic editing commands

(akirak/bind-kill
  "f" 'flush-lines
  "l" 'delete-blank-lines)

(akirak/bind-replace
  "p" #'projectile-replace
  "M-p" #'projectile-replace-regexp)

(use-package adapted
  :straight (adapted :host github :repo "akirak/adapted")
  :config
  (require 'adapted-setup)
  (adapted-setup-hooks)
  :general
  (:keymaps 'adapted-mode-map :package 'adapted
            "C-c f" #'adapted-fix-at-point
            "C-c r" #'adapted-find-references-at-point
            "C-c q" #'adapted-format-region
            [help ?.] #'adapted-documentation-at-point
            "M-." #'adapted-jump-to-definition
            "M-," #'adapted-jump-back
            "C-. r ." #'adapted-refactor
            "C-. r s" #'adapted-rename-symbol
            "C-. r f" #'adapted-rename-file
            "C-. f f" #'adapted-format-region
            "C-. f o" #'adapted-organize-imports
            "C-. ?" #'adapted-error-at-point
            "C-. i b" #'adapted-insert-block-comment)
  :custom
  (adapted-documentation-at-point-fallback #'helpful-at-point))

(use-package adapted-avy
  :straight adapted
  :custom
  (adapted-avy-prefix-key "C-;"))

(akirak/bind-file-extra
  "e" #'adapted-error-list)

(provide 'setup-edit)

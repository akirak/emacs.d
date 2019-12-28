;; Mode-agnostic editing commands

(akirak/bind-kill
  "f" 'flush-lines
  "l" 'delete-blank-lines)

(akirak/bind-replace
  "p" #'projectile-replace
  "M-p" #'projectile-replace-regexp)

(provide 'setup-edit)

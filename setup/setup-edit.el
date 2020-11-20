;; Mode-agnostic editing commands

(akirak/bind-generic
  "k f" 'flush-lines
  "k l" 'delete-blank-lines)

(akirak/bind-generic
  "r p" #'projectile-replace
  "r M-p" #'projectile-replace-regexp)

(provide 'setup-edit)

(use-package multiple-cursors)

(general-def
  "<S-up>" #'mc/mark-previous-like-this
  "<S-down>" #'mc/mark-next-like-this)

(akirak/bind-user
  "|" #'mc/edit-lines)

(provide 'setup-multiple-cursors)

(use-package undo-fu
  :general
  ("C-z" #'undo-fu-only-undo
   "C-S-z" #'undo-fu-only-redo))

(provide 'setup-undo)

(akirak/bind-file-extra
  "M" '(nil :wk "chmod")
  "Mx" #'executable-set-magic
  "MM" #'set-file-modes
  "R" #'revert-buffer-with-coding-system)

(use-package trashed
  :straight (:host github :repo "shingo256/trashed"))

(provide 'setup-files)

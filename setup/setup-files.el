(akirak/bind-file-extra
  "c" #'revert-buffer-with-coding-system
  ;; "S" #'sudo-find-file
  "S" #'su-mode
  "D" #'crux-delete-file-and-buffer
  "R" #'crux-rename-file-and-buffer
  "M" #'set-file-modes
  "X" #'executable-set-magic
  "!" #'crux-open-with)

(use-package executable
  :straight nil
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))

(use-package trashed
  :straight (:host github :repo "shingo256/trashed"))

(provide 'setup-files)

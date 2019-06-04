(use-package magit
  :config
  (when (fboundp 'unpackaged/magit-log-date-headers-mode)
    (unpackaged/magit-log-date-headers-mode 1))
  :general
  ;; C-c M-g is bound to magit-file-dispatch by default.
  ;; I will bind C-c M-KEY to some other frequently used magit commands.
  ("C-S-g" #'magit-dispatch
   "<S-f7>" #'magit-branch-checkout)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  ;; Automatically save file buffers in the repository
  (magit-save-repository-buffers (quote dontask)))

(provide 'setup-magit)

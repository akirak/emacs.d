(use-package magit
  :config
  (when-let ((bin (executable-find "git")))
    (setq magit-git-executable bin))
  (when (fboundp 'unpackaged/magit-log-date-headers-mode)
    (unpackaged/magit-log-date-headers-mode 1))
  :general
  ;; C-c M-g is bound to magit-file-dispatch by default.
  ;; I will bind C-c M-KEY to some other frequently used magit commands.
  ("C-S-g" #'magit-dispatch
   "<S-f7>" #'magit-branch-checkout)
  :custom
  (magit-display-buffer-function
   (if akirak/to-be-run-as-exwm
       'magit-display-buffer-same-window-except-diff-v1
     'magit-display-buffer-fullframe-status-v1))
  ;; Automatically save file buffers in the repository
  (magit-save-repository-buffers (quote dontask)))

(provide 'setup-magit)

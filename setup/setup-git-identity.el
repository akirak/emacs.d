(use-package git-identity
  :after magit
  :straight (git-identity :host github :repo "akirak/git-identity.el")
  :config
  (git-identity-magit-mode 1)
  :general
  (:keymaps 'magit-status-mode-map :package 'magit
            "I" #'git-identity-info))

(provide 'setup-git-identity)

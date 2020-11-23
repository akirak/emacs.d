(use-package git-identity
  :after magit
  :straight (git-identity :host github :repo "akirak/git-identity.el")
  :general
  (:keymaps 'magit-status-mode-map :package 'magit
            "I" #'git-identity-info)
  :custom
  (git-identity-magit-mode t))

(provide 'setup-repos)

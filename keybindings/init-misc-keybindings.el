(general-def
  "<menu>" #'which-key-show-major-mode
  "<pause>" #'counsel-org-offtime)

(general-def
  "<M-prior>" #'git-gutter:previous-hunk
  "<M-next>" #'git-gutter:next-hunk
  "<M-insert>" #'git-gutter:stage-hunk
  "<M-S-insert>" #'magit-stage-file
  "<C-insert>" #'magit-commit-popup
  "<M-home>" #'magit
  "<M-end>" #'magit-branch-checkout)

(provide 'init-misc-keybindings)

(general-def
  "<M-prior>" #'git-gutter:previous-hunk
  "<M-next>" #'git-gutter:next-hunk
  "<M-insert>" #'git-gutter:stage-hunk
  "<M-S-insert>" #'magit-stage-file
  "<M-delete>" #'git-gutter:revert-hunk)

(provide 'init-misc-keybindings)

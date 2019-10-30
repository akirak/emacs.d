(use-package forge
  :config
  (akirak/bind-browse-at-remote
    "w" #'forge-browse-dwim
    "RET" #'forge-browse-post
    "c" #'forge-browse-commit
    "t" #'forge-browse-topic
    "I" #'forge-browse-issues
    "P" #'forge-browse-pullreqs))

(use-package github-review
  :commands (github-review-forge-pr-at-point))

(provide 'setup-github)

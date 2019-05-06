(use-package forge
  :disabled t
  :straight (forge :host github :repo "magit/forge"))

(use-package github-review
  :commands (github-review-forge-pr-at-point))

(provide 'setup-github)

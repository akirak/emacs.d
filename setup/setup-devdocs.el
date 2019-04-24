(use-package devdocs-lookup
  :straight (devdocs-lookup-kmodi :host github :repo "kaushalmodi/devdocs-lookup")
  :commands (devdocs-lookup)
  :config
  (akirak/bind-generic "d" #'devdocs-lookup))

(provide 'setup-devdocs)

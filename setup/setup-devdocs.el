(use-package devdocs-lookup
  :straight (devdocs-lookup :host github :repo "skeeto/devdocs-lookup")
  :commands (devdocs-lookup)
  :config
  (akirak/bind-generic "d" #'devdocs-lookup))

(provide 'setup-devdocs)

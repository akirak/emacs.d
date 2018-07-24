(straight-override-recipe '(org-super-agenda
                            :host github
                            :repo "akirak/org-super-agenda"
                            :branch "reverse-auto-category-items"))

(use-package org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode 1))

(provide 'init-org-super-agenda)

(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter")
  :functions (org-starter-define-directory org-starter-define-file)
  :custom
  (org-starter-exclude-from-recentf '(known-files path)))

(use-package helm-org-starter
  :straight org-starter
  :commands (helm-org-starter))

(provide 'init-org-starter)

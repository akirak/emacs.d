(use-package org-starter
  :straight (org-starter :host github :repo "akirak/org-starter")
  :functions (org-starter-define-directory org-starter-define-file)
  :custom
  (org-starter-exclude-from-recentf '(known-files path)))

(use-package helm-org-starter
  :straight org-starter
  :commands (helm-org-starter)
  :defines (helm-org-starter-known-file-source))

(use-package counsel-org-starter
  :straight org-starter
  :commands (counsel-org-starter counsel-org-starter-known-file))

(provide 'init-org-starter)

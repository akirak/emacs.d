(use-package org-offtime
  :after org
  :straight (org-offtime :host github :repo "akirak/org-offtime")
  :config
  (add-hook 'org-offtime-hook 'bookmark-save)
  (add-hook 'org-offtime-hook 'org-store-agenda-views))

(provide 'init-org-offtime)

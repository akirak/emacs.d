(use-package org-recent-headings
  :config
  (org-recent-headings-mode 1)
  :custom
  (org-recent-headings-save-file (expand-file-name ".cache/org-recent-headings"
                                                   user-emacs-directory)))

(provide 'init-org-recent-headings)

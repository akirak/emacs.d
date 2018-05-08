(use-package org-download
  :custom
  (org-download-edit-cmd "pinta %s")
  (org-download-screenshot-method "scrot -s %s"))

(provide 'init-org-download)

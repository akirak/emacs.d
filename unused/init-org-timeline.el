(use-package org-timeline
  :after org
  :straight (org-timeline :host github :repo "Fuco1/org-timeline")
  :hook
  (org-agenda-finalize . org-timeline-insert-timeline))

(provide 'init-org-timeline)

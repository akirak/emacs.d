(use-package offtime
  :straight (org-offtime :host github :repo "akirak/org-offtime")
  :custom
  (offtime-lock-command "sudo physlock")
  (offtime-suspend-command "sudo systemctl suspend"))

(use-package org-offtime
  :straight org-offtime)

(provide 'setup-org-offtime)

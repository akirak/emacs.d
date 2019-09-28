(use-package org-edna
  :after org
  :straight (org-edna :host github :repo "akirak/org-edna" :branch "edit")
  :init
  (defun akirak/org-edna-edit-setup-company ()
    (setq company-backends '(company-capf))
    (company-mode 1))
  (org-edna-load)
  :hook
  (org-edna-edit-mode . akirak/org-edna-edit-setup-company))

(provide 'setup-org-edna)

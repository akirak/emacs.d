(use-package org-edna
  :after org
  :straight (org-edna :host github :repo "akirak/org-edna" :branch "edit")
  :config
  (defun akirak/org-edna-edit-setup-company ()
    (setq company-backends '(company-capf))
    (company-mode 1))
  (defvar org-edna-edit-mode-hook nil)
  (org-edna-load)
  (advice-add 'org-edna-edit
              :after (lambda (&rest _args) (run-hooks 'org-edna-edit-mode-hook)))
  :hook
  (org-edna-edit-mode . akirak/org-edna-edit-setup-company))

(provide 'setup-org-edna)

(use-package org-edna
  :after org
  :straight (org-edna :host github :repo "akirak/org-edna")
  :config
  (org-edna-load)
  (setq-mode-local org-edna-edit-mode company-backends '(company-capf))
  (defun akirak/helm-org-rifle-add-edna-blocker-with-id (candidate)
    (unless (derived-mode-p 'org-mode 'org-agenda-mode)
      (user-error "Not in org-mode or org-agenda-mode"))
    (let ((id (with-current-buffer (car candidate)
                (org-with-wide-buffer
                 (goto-char (cdr candidate))
                 (org-id-get-create t)))))
      (akirak/org-edna-add-id-blocker id)))
  :hook
  (org-edna-edit-mode . company-mode))

(use-package org-edna-avy
  :straight org-edna)

(provide 'setup-org-edna)

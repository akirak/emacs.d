(use-package org-journal
  :after org-starter
  :config
  (akirak-org-journal-setup)

  (defun akirak/helm-org-ql-journal ()
    (interactive)
    (helm-org-ql (nreverse (akirak-org-journal-files))))
  (add-to-list 'org-starter-extra-alternative-find-file-map
               '("j" akirak/helm-org-ql-journal "org-journal"))
  (general-unbind "C-c C-j")

  (akirak/bind-search
    "M-j" #'org-journal-search)

  (akirak/bind-jump
    "M-d" #'org-journal-new-date-entry)

  (add-hook 'org-journal-mode-hook
            (defun akirak/org-journal-entry-init ()
              (setq-local org-multi-wiki-want-custom-id nil)))

  :general
  (:keymaps 'org-journal-mode-map
            ;; Use the same values as in org-mode-map
            "C-c C-f" #'org-forward-heading-same-level
            "C-c C-b" #'org-backward-heading-same-level)

  :custom
  (org-journal-find-file #'find-file)
  (org-journal-date-format "%F (%a)")
  (org-journal-file-format "%Y%m%d.org"))

(provide 'setup-org-journal)

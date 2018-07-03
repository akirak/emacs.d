(general-def :keymaps 'org-mode-map :package 'org
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-c o l" '(org-web-tools-insert-link-for-url :wk "insert link for url")
  "C-1" 'counsel-org-tag
  ;; TODO: Create hydra commands
  ;; "C-2" 'org-time-hydra
  ;; "C-3" 'org-edna-hydra
  "C-4" 'org-refile-hydra
  "C-6" 'akirak/org-export-subtree-to-hugo-dwim
  ;; "C-8" 'org-insert-hydra
  )

(provide 'init-org-bindings)

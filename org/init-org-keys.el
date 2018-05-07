(general-def :keymaps 'org-mode-map :package 'org
  [remap org-set-tags-command] 'counsel-org-tag
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-c o l" '(org-web-tools-insert-link-for-url :wk "insert link for url"))

(provide 'init-org-keys)

(general-def :keymaps 'org-mode-map :package 'org
  ;; Tweak the binding to org-set-tags-command.
  ;; If two universal prefixes are given, run counsel-org-tag.
  ;; Otherwise the same as the default.
  "C-c C-q" '((lambda (&optional arg)
                (interactive "P")
                (if (eql arg '(16))
                    (counsel-org-tag)
                  (org-set-tags-command arg)))
              :wk "set org tags")
  "M-n" 'org-metadown
  "M-p" 'org-metaup
  "M-H" 'org-shiftmetaleft
  "M-L" 'org-shiftmetaright
  "C-c o l" '(org-web-tools-insert-link-for-url :wk "insert link for url"))

(provide 'init-org-bindings)

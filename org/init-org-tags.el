(setq-default org-use-fast-tag-selection t
              org-fast-tag-selection-single-key nil
              org-group-tags t
              org-agenda-use-tag-inheritance t
              org-tags-exclude-from-inheritance
              '(
                ;; "objective"
                ;; "sprint"
                ;; "agenda-group"
                ))

(provide 'init-org-tags)

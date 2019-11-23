(use-package counsel-org-clock
  :custom
  ;; You have to define 'd' agenda in somewhere.
  ;; It can be contextual, i.e. multiple commands with contexts.
  (counsel-org-clock-goto-fallback-function #'akirak/org-journal-open-today)
  (counsel-org-clock-default-action 'clock-in))

(provide 'setup-counsel-org-clock)

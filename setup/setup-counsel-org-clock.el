(use-package counsel-org-clock
  :custom
  ;; You have to define 'd' agenda in somewhere.
  ;; It can be contextual, i.e. multiple commands with contexts.
  (counsel-org-clock-goto-fallback-function (lambda () (org-agenda nil "d")))
  (counsel-org-clock-default-action 'clock-in))

(provide 'setup-counsel-org-clock)

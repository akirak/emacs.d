(use-package olivetti
  :commands (turn-on-olivetti-mode)
  :hook
  (org-mode . turn-on-olivetti-mode)
  (markdown-mode . turn-on-olivetti-mode)
  :custom
  (olivetti-body-width 92))

(provide 'init-olivetti)

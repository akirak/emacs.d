(use-package real-auto-save
  :hook
  (org-mode . real-auto-save-mode)
  :custom
  (real-auto-save-interval 30))

(provide 'init-autosave)

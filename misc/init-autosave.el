(use-package real-auto-save
  :config
  (defun akirak/turn-on-auto-save-unless-olivetti ()
    (real-auto-save-mode (if (bound-and-true-p olivetti-mode) nil 1)))
  :hook
  (org-mode . real-auto-save-mode)
  (olivetti . akirak/turn-on-auto-save-unless-olivetti)
  :custom
  (real-auto-save-interval 30))

(provide 'init-autosave)

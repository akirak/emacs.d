(setq midnight-mode t
      clean-buffer-list-delay-general 1)

(with-eval-after-load 'midnight
  (add-to-list 'clean-buffer-list-kill-regexps "\\*helm")
  (add-to-list 'clean-buffer-list-kill-regexps "\\*magit"))

(add-hook 'midnight-hook 'org-store-agenda-views)

(provide 'init-midnight)

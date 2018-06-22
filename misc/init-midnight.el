(setq midnight-mode t
      clean-buffer-list-delay-general 2)

(with-eval-after-load 'midnight
  (add-to-list 'clean-buffer-list-kill-regexps "\\*helm"))

(add-hook 'midnight-hook 'org-store-agenda-views)

(provide 'init-midnight)

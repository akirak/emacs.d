(setq midnight-mode t
      clean-buffer-list-delay-general 2)

(with-eval-after-load 'midnight
  (add-to-list 'clean-buffer-list-kill-regexps "\\*helm"))

(provide 'init-midnight)

(use-package slack
  :commands (slack-start)
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t))

(provide 'setup-slack)

(use-package css-eldoc
  :after css-mode
  :commands (turn-on-css-eldoc)
  :hook
  (css-mode . turn-on-css-eldoc))

(provide 'setup-css)

(use-package beancount
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook
  (beancount-mode . smartparens-strict-mode))

(provide 'setup-beancount)

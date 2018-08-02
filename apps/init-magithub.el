(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject 'status-checks-header))

(provide 'init-magithub)

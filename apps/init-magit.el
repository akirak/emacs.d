(use-package magit
  :config
  (require 'init-magit-repolist nil t)
  :custom
  (magit-save-repository-buffers (quote dontask)))

(provide 'init-magit)

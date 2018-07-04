(use-package magit
  :config
  (require 'init-magit-repolist nil t)
  :custom
  (magit-save-repository-buffers (quote dontask)))

;;;; Extra sections in =magit-status=
;;;;; magit-todos
;; magit-todos requires hl-todo
(require 'init-hl-todo)
(use-package magit-todos
  :after magit
  :straight (magit-todos :host github :repo "alphapapa/magit-todos")
  :config
  (magit-todos-mode 1))

;;;;; magithub
(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject 'status-checks-header))

(provide 'init-magit)

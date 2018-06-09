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
  ;; Disabled for now, as it slows down magit
  :straight (magit-todos :host github :repo "alphapapa/magit-todos")
  :config
  (magit-todos-mode 1))

(provide 'init-magit)

(use-package helm-make
  :general
  (:package 'projectile :keymaps 'projectile-command-map
            "m" 'helm-make-projectile))

(provide 'init-helm-make)

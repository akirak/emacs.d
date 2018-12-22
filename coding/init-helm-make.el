(use-package helm-make :after helm
  :general
  (:package 'projectile :keymaps 'projectile-command-map
            "m" 'helm-make-projectile))

(provide 'init-helm-make)

(use-package helm-systemd :after helm
  :commands (helm-systemd))

(akirak/bind-admin
  "s" '(nil :wk "systemd")
  "sh" 'helm-systemd)

;; Manage docker services
(use-package docker
  :disabled t
  :config
  (akirak/bind-admin
    "k" '(nil :wk "docker")
    "ki" 'docker-images
    "kk" 'docker-containers))

;; Manage daemons
(use-package prodigy)

(akirak/bind-admin
  "p" '(nil :wk "processes")
  "pp" #'prodigy
  "pt" #'helm-top)

(provide 'setup-services)

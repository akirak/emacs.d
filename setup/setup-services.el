(use-package helm-systemd :after helm
  :commands (helm-systemd))

;; Manage docker services
(use-package docker
  :disabled t)

;; Manage daemons
(use-package prodigy
  :disabled t)

(provide 'setup-services)

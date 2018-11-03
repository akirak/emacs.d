(define-prefix-command 'akirak/app-map)

(general-def akirak/app-map
  "c" 'calendar
  "d" 'helm-linux-disks
  "i" 'docker-images
  "k" 'docker-containers
  "p" 'prodigy
  "P" 'helm-system-packages
  "S" 'helm-systemd
  "t" 'helm-top
  "u" 'uptimes)

(provide 'init-app-map)

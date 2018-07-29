(define-prefix-command 'akirak/app-map)

(general-def akirak/app-map
  "a" 'org-agenda
  "c" 'calendar
  "d" 'helm-linux-disks
  "i" 'docker-images
  "j" 'org-journal-new-entry
  "k" 'docker-containers
  "p" 'prodigy
  "P" 'helm-system-packages
  "S" 'helm-systemd
  "t" 'helm-top
  "u" 'uptimes)

(provide 'init-app-map)

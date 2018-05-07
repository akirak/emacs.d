(define-prefix-command 'akirak/app-map)

(general-def akirak/app-map
  ;; "a" '((lambda () (interactive) (org-agenda nil "a")) :which-key "agenda")
  "d" 'helm-linux-disks
  "i" 'docker-images
  "k" 'docker-containers
  "p" 'prodigy
  "t" 'helm-top)

(provide 'init-app-map)

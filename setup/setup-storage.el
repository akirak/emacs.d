(use-package disk-usage)

(use-package helm-linux-disks
  :straight (helm-linux-disks :host github
                              :repo "akirak/helm-linux-disks")
  :commands (helm-linux-disks)
  :custom
  (linux-disk-terminal-type 'akirak/shell-new))

(akirak/bind-admin
  "d" '(nil :wk "disks")
  "du" #'disk-usage
  "dh" #'helm-linux-disks)

(provide 'setup-storage)

(use-package helm-linux-disks
  :straight (helm-linux-disks :host github
                              :repo "akirak/helm-linux-disks")
  :commands (helm-linux-disks)
  :custom
  (linux-disk-terminal-type 'aweshell-new))

(provide 'init-linux-disk)

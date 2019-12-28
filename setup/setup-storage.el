(use-package disk-usage
  :general
  (:keymaps 'akirak/admin-map
            "D" 'disk-usage))

(use-package helm-linux-disks
  :straight (helm-linux-disks :host github
                              :repo "akirak/helm-linux-disks")
  :commands (helm-linux-disks)
  :custom
  (linux-disk-terminal-type 'akirak/shell-new))

(provide 'setup-storage)

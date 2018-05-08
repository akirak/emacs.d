(use-package helm-linux-disks
  :disabled t
  :straight (helm-linux-disks :host github :repo "akirak/helm-linux-disks")
  :custom
  (linux-disk-terminal-type (quote multi-term)))

(provide 'init-linux-disk)

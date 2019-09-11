(use-package gcmh
  :straight (gcmh :host github :repo "akirak/gcmh"
                  :branch "logging")
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 15))

(provide 'setup-gc)

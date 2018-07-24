;; I prefer Ivy for most situations, but Helm is better in some situations
;; especially where multiple sources are involved.

(use-package helm
  :config
  (helm-autoresize-mode 1)
  (require 'helm-mini-extra)
  :custom
  ;; (helm-command-prefix-key "C-c h")
  (helm-autoresize-max-height 40)
  (helm-display-function (quote pop-to-buffer))
  ;; (helm-split-window-in-side-p t)
  :general
  ([remap apropos-command] 'helm-apropos))

(use-package helm-system-packages
  :commands (helm-system-packages))

(use-package helm-systemd
  :commands (helm-systemd))

(provide 'init-helm)

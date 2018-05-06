;; I prefer Ivy for most situations, but Helm is better in some situations
;; especially where multiple sources are involved.

(use-package helm
  :config
  (helm-autoresize-mode 1)
  :custom
  ;; (helm-command-prefix-key "C-c h")
  (helm-autoresize-max-height 40)
  ;; (helm-split-window-in-side-p t)
  )

(provide 'init-helm)

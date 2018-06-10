(diminish 'outline-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)

(use-package outshine
  :general
  (:keymaps 'outline-minor-mode-map
            "C-M-i" nil)
  :hook
  (outline-minor-mode . outshine-hook-function)
  :custom
  (outshine-use-speed-commands t))

(use-package navi-mode)

;; Jumping based on outlines (better than imenu)
(use-package helm-navi :after navi-mode)

(provide 'init-outshine)

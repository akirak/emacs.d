(use-package rich-minority
  :straight (rich-minority :host github :repo "Malabarba/rich-minority")
  :init
  (setq rm-whitelist (rx " LY"))
  (rich-minority-mode 1))

(provide 'setup-rich-minority)

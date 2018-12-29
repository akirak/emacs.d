(use-package ivy-filthy-rich
  :straight (ivy-filthy-rich :host github :repo "akirak/ivy-filthy-rich"
                             :branch "fix-max-length")
  :diminish 'ivy-filthy-rich-mode
  :after ivy
  :config
  (ivy-filthy-rich-mode t)
  :custom
  (ivy-filthy-rich-max-length 120))

;; I once used this package, but I am now using ivy-filthy-rich instead.
(use-package ivy-rich
  :after ivy
  :disabled t
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(provide 'setup-ivy-filthy-rich)
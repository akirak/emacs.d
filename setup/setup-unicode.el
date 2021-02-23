(use-package pcache)

(use-package persistent-soft
  :requires pcache
  :straight (:host github :repo "rolandwalker/persistent-soft"))

(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(provide 'setup-unicode)

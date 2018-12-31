(use-package doom-modeline
  :straight (doom-modeline :host github :repo "seagle0128/doom-modeline")
  :hook
  (after-init . #'doom-modeline-init))

(provide 'init-doom-modeline)

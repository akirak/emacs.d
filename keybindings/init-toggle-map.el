;; TODO: Replace this with a hydra

(define-prefix-command 'akirak/toggle-map)

(general-def akirak/toggle-map
  "c" 'flycheck-mode
  "d" 'eldoc-mode
  "s" 'ispell-minor-mode
  "t" 'treemacs)

(provide 'init-toggle-map)

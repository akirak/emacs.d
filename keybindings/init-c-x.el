(require 'init-customize-map)
(require 'init-toggle-map)

(general-def
  "C-x c" #'akirak/customize-map
  "C-x g" #'magit
  "C-x p" #'counsel-projectile
  "C-x t" #'akirak/toggle-map
  "C-x u" #'undo-tree-visualize
  "C-x K" #'kill-this-buffer
  "C-x /" #'counsel-ag
  "C-x =" #'narrow-or-widen-dwim
  "C-x C-b" #'ibuffer
  "C-x C-j" #'dired-jump)

(provide 'init-c-x)

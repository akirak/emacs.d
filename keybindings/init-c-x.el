(require 'init-customize-map)

(general-def
  "C-x c" #'akirak/customize-map
  "C-x g" #'magit
  "C-x p" #'counsel-projectile
  "C-x t" #'helm-tail
  "C-x u" #'undo-tree-visualize
  "C-x k" #'kill-this-buffer
  "C-x w" #'frame-workflow-map
  "C-x /" #'counsel-ag
  "C-x =" #'narrow-or-widen-dwim
  "C-x C-b" #'ibuffer
  "C-x C-j" #'dired-jump)

(provide 'init-c-x)

(require 'init-customize-map)

(general-def
  "C-x D" #'crux-delete-file-and-buffer
  "C-x F" #'counsel-recentf
  "C-x L" #'counsel-locate
  "C-x R" #'crux-rename-file-and-buffer
  "C-x S" #'sudo-find-file
  "C-x T" #'akirak/shell-toggle-dedicated
  "C-x c" #'akirak/customize-map
  "C-x k" #'kill-this-buffer
  "C-x p" #'counsel-projectile
  "C-x t" #'helm-tail
  "C-x x" #'crux-open-with
  "C-x /" #'counsel-rg
  "C-x =" #'narrow-or-widen-dwim
  "C-x C-b" #'ibuffer
  "C-x C-j" #'dired-jump)

(provide 'init-c-x)

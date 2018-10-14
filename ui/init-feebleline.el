(require 'init-magit)

(use-package feebleline
  :straight (feebleline :host github :repo "akirak/feebleline")
  :init
  (require 'magit nil t)
  (feebleline-mode 1)
  :custom
  (feebleline-show-git-branch t)
  (feebleline-show-dir t)
  (feebleline-show-time t)
  (feebleline-time-format "[%b %d %a %H:%M:%S]")
  (feebleline-show-previous-buffer t))

(provide 'init-feebleline)

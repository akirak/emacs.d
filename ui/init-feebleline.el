(use-package feebleline
  :straight (feebleline :host github :repo "akirak/feebleline")
  :init
  (feebleline-mode 1)
  :custom
  (feebleline-show-git-branch t)
  (feebleline-show-dir t)
  (feebleline-show-time t)
  (feebleline-time-format "[%b %d %a %H:%M:%S]")
  (feebleline-show-previous-buffer t))

(provide 'init-feebleline)

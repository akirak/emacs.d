(use-package multi-term
  :custom
  (multi-term-dedicated-select-after-open-p t)
  (multi-term-dedicated-window-height 20))

(akirak/define-frame-workflow "terminal"
  :key "t"
  :make-frame
  '(frame-purpose-make-frame :modes '(term-mode
                                      eshell-mode
                                      shell-mode))
  :layout
  '(ibuffer-sidebar-show-sidebar))

(provide 'init-multi-term)

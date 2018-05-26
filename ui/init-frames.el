(unless (require 'dash-functional nil t)
  (use-package dash-functional))

(use-package frame-purpose
  :straight (frame-purpose :host github :repo "alphapapa/frame-purpose.el")
  :config
  (frame-purpose-mode 1))

(use-package frame-workflow
  :straight (frame-workflow :host github :repo "akirak/frame-workflow")
  :config
  (require 'frame-workflow-purpose)
  (frame-workflow-purpose-setup)
  (require 'frame-workflow-menu))

(provide 'init-frames)

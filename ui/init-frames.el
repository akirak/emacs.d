(unless (require 'dash-functional nil t)
  (use-package dash-functional))

(use-package frame-purpose
  :straight (frame-purpose :host github :repo "alphapapa/frame-purpose.el")
  :config
  (frame-purpose-mode 1))

(use-package frame-workflow
  :straight (frame-workflow :host github :repo "akirak/frame-workflow")
  :init
  (setq projectile-switch-project-action
        #'frame-workflow-switch-directory-frame)
  :config
  (frame-workflow-mode 1))

(use-package helm-frame-workflow
  :after (frame-workflow helm)
  :straight frame-workflow
  :commands (helm-frame-workflow))

;;;; Some workspaces
(akirak/define-frame-workflow "emacs-config"
  :key "C"
  :layout
  '(progn
     (delete-other-windows)
     (when (fboundp 'ibuffer-sidebar-show-sidebar)
       (ibuffer-sidebar-show-sidebar))
     (find-file (expand-file-name "emacs.org" user-emacs-directory))
     (split-window-sensibly)
     (frame-workflow-magit-same-window))
  :make-frame
  '(frame-purpose-make-directory-frame user-emacs-directory))

(akirak/define-frame-workflow "home"
  :key "h"
  :layout '(corefighter-sidebar))

(provide 'init-frames)

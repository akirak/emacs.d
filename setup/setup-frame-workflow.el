(add-to-list 'default-frame-alist '(fullscreen . maximized))

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

(defalias 'akirak/define-frame-workflow 'frame-workflow-define-subject)
(make-obsolete 'akirak/define-frame-workflow
               'frame-workflow-define-subject "2019.1.1")

(use-package helm-frame-workflow :after (frame-workflow helm)
  :straight frame-workflow
  :commands (helm-frame-workflow))

(provide 'setup-frame-workflow)

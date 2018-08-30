(require 'init-org)
(require 'init-frames)

(use-package my-org
  :straight (my-org :host github :repo "akirak/my-org")
  :commands (akirak/helm-org-rifle-knowledge-base)
  :init
  (akirak/define-frame-workflow "org"
    :key "o"
    :layout '(when (fboundp #'ibuffer-sidebar-show-sidebar)
               (ibuffer-sidebar-show-sidebar))
    :make-frame '(frame-purpose-make-mode-frame 'org-mode))
  :general
  ("<menu> <menu>" #'akirak/helm-org-rifle-knowledge-base))

(dolist (file (directory-files (expand-file-name "local" user-emacs-directory)
                               t "^my-.+\\.el"))
  (condition-case err
      (require (intern (file-name-base file)) file)
    (error (message "%s was not loaded properly: %s" file err))))

(provide 'init-local)

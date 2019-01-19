(autoload 'helm-org-rifle-files "helm-org-rifle")

(use-package org-starter
  :functions (org-starter-define-directory org-starter-define-file)
  :init
  (org-starter-mode 1)
  :config
  ;; Prevent an error that can be caused when a custom agenda command
  ;; is defined by org-starter
  (defvar org-agenda-custom-commands nil)
  (defun helm-org-rifle-known-files ()
    (interactive)
    (helm-org-rifle-files org-starter-known-files))
  :custom
  ;; `org-starter-initial-capture-templates` is defined in setup-org-capture.el
  (org-starter-require-file-by-default nil)
  (org-starter-exclude-from-recentf '(known-files path))
  (org-starter-enable-local-variables :all))

(provide 'setup-org-starter)

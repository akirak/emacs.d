(use-package org-make-toc
  :after (org)
  :config
  (defvar org-make-toc nil
    "If non-nil, turn on `org-make-toc-mode' in the buffer.")
  (make-variable-buffer-local 'org-make-toc)
  (defun maybe-turn-on-org-make-toc-mode ()
    (when org-make-toc
      (org-make-toc-mode 1)))
  :hook
  (org-mode . maybe-turn-on-org-make-toc-mode))

(provide 'init-org-make-toc)

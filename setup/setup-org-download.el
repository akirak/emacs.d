(use-package org-download
  :config
  (cond
   ((and (eq system-type 'gnu/linux)
         (eq window-system 'x))
    (setq-default org-download-screenshot-method "flameshot gui --raw > %s")))
  :custom
  (org-download-image-dir "image")
  (org-download-edit-cmd "pinta %s"))

(provide 'setup-org-download)

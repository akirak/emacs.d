;;; init-org-enhance.el --- Enhancements to org mode -*- lexical-binding: t -*-

;; Prettify headline bullets
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Edit Org-Mode lists like in word processors
(use-package org-autolist
  :diminish 'org-autolist-mode
  :init
  (add-hook 'org-mode-hook #'org-autolist-mode))

;; Allow you to bookmark headings in Org-Mode
(use-package org-bookmark-heading)

(provide 'init-org-enhance)
;;; init-org-enhance.el ends here

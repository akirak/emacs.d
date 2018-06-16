;;; init-org-enhance.el --- Enhancements to org mode -*- lexical-binding: t -*-

;; https://emacs.stackexchange.com/questions/21171/company-mode-completion-for-org-keywords
(defun org-add-completion-at-point ()
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point
            nil t))
(add-hook 'org-mode-hook #'org-add-completion-at-point)

;; Prettify headline bullets
(use-package org-bullets
  :disabled t
  :after org
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; Edit Org-Mode lists like in word processors
(use-package org-autolist
  :after org
  :diminish 'org-autolist-mode
  :init
  (add-hook 'org-mode-hook #'org-autolist-mode))

;; Allow you to bookmark headings in Org-Mode
(use-package org-bookmark-heading
  :after org)

(provide 'init-org-enhance)
;;; init-org-enhance.el ends here

;; The following packages are enhancements to Emacs that don't require
;; much configuration

;; Install smex for sorting M-x candidates
(use-package smex
  ;; Disabled in favour of prescient
  :disabled t)
;; Save minibuffer history
(use-package savehist)

(use-package helm-tail :after helm
  :straight (helm-tail :host github :repo "akirak/helm-tail")
  :commands (helm-tail))

(use-package scratch
  :commands (scratch)
  :config
  (add-to-list 'scratch-mode-alist '(helpful-mode . emacs-lisp-mode))
  (add-to-list 'scratch-mode-alist '(help-mode . emacs-lisp-mode)))
(use-package iedit)
(use-package narrow-or-widen
  :straight nil
  :load-path "contrib/misc")
;; Edit Org-Mode lists like in word processors
(use-package org-autolist
  :after org
  ;; :diminish 'org-autolist-mode
  :init
  (add-hook 'org-mode-hook #'org-autolist-mode))
;; Allow you to bookmark headings in Org-Mode
(use-package org-bookmark-heading
  :after org)
(use-package org-bullets :after org
  :disabled t
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))
;; Exporting mind maps
(use-package akirak/org-refile
  :straight nil)
(use-package beginend
  :config
  (beginend-global-mode 1))
(use-package executable
  :straight nil
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p))
(use-package deadgrep
  :commands deadgrep
  :config
  (akirak/bind-search "M-p" #'deadgrep))
(use-package align
  :general
  (:keymaps 'akirak/align-prefix-map
            "a" 'align))
(use-package validate
  :straight (validate :host github :repo "Malabarba/validate.el"))
(use-package git-link)

(provide 'setup-misc)

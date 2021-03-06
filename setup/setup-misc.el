;; The following packages are enhancements to Emacs that don't require
;; much configuration
(use-package savehist)

(use-package scratch
  :commands (scratch)
  :config
  (add-to-list 'scratch-mode-alist '(helpful-mode . emacs-lisp-mode))
  (add-to-list 'scratch-mode-alist '(help-mode . emacs-lisp-mode)))
(use-package narrow-or-widen
  :straight nil
  :load-path "contrib/misc")
;; Edit Org-Mode lists like in word processors
;; Exporting mind maps
(use-package akirak/org-refile
  :straight nil)
(use-package beginend
  :config
  (beginend-global-mode 1))

(use-package align
  :general
  (:keymaps 'akirak/align-prefix-map
            "a" 'align))
(use-package git-link
  :custom
  (git-link-use-commit t))

(provide 'setup-misc)

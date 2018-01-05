(setq-default version-control t
              vc-follow-symlinks t
              vc-make-backup-files t
              delete-old-versions -1
              coding-system-for-read 'utf-8
              coding-system-for-write 'utf-8
              sentence-end-double-space nil
              default-fill-column 80
              initial-scratch-message ";;; Emacs Lisp scratch buffer\n"

              backup-directory-alist
              `("." ,(concat akirak/emacs-cache-directory "/backup"))
              auto-save-file-name-transforms
              `((".*" ,(concat akirak/emacs-cache-directory "/auto-save-list/") t))
              )

(use-package restart-emacs
  :straight t)

(use-package emacs-pg
  :straight (emacs-pg :type git :host github :repo "akirak/emacs-pg")
  :defer t
  :commands (emacs-pg-try
             emacs-pg-add
             emacs-pg-adopt
             emacs-pg-dismiss
             emacs-pg-update-symlinks))

(provide 'ak-base)

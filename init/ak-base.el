(setq-default version-control t
              vc-follow-symlinks t
              vc-make-backup-files t
              coding-system-for-read 'utf-8
              coding-system-for-write 'utf-8
              sentence-end-double-space nil
              default-fill-column 80
              initial-scratch-message ";;; Emacs Lisp scratch buffer\n")

(setq recentf-save-file "~/.cache/emacs/recentf")

(use-package restart-emacs
  :straight t)

(use-package play
  :straight (play :type git :host github :repo "akirak/play.el")
  :defer t
  :commands (play-checkout
             play-start-last
             play-adopt
             play-dismiss
             play-update-symlinks))

(provide 'ak-base)

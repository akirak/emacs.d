(use-package treemacs
  :preface
  (setq treemacs-python-executable (executable-find "python3"))
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (setq treemacs-git-mode 'deferred))
    (`(t . _)
     (setq treemacs-git-mode 'simple)))
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 1))

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :config
  (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package treemacs-projectile
  :disabled t
  :init
  (defun akirak/treemacs-projectile (&optional arg)
    (interactive)
    (if arg
        (treemacs-projectile-toggle)
      (treemacs-projectile))))

(provide 'setup-treemacs)

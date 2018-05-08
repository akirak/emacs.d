(use-package treemacs)

(use-package treemacs-projectile
  :disabled t
  :init
  (defun akirak/treemacs-projectile (&optional arg)
    (interactive)
    (if arg
        (treemacs-projectile-toggle)
      (treemacs-projectile))))

(provide 'init-treemacs)

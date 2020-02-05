;; Both ibuffer-projectile and ibuffer-vc are similar packages,
;; but I prefer ibuffer-projectile for now.

(use-package ibuffer-projectile
  :after projectile
  :disabled t
  :config
  (defun ibuffer-projectile-run ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  (ibuffer . ibuffer-projectile-run)
  :custom
  (ibuffer-projectile-group-name-function
   (lambda (_name root) (abbreviate-file-name root))))

(use-package ibuffer-vc
  :disabled t
  :config
  (defun akirak/ibuffer-vc ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic)))
  :hook
  (ibuffer . akirak/ibuffer-vc))

(use-package ibuffer-project
  :straight (:host github :repo "akirak/emacs-ibuffer-project"
                   :branch "tramp")
  :config
  (defun ibuffer-project-setup ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))
  :hook
  (ibuffer . ibuffer-project-setup))

(provide 'setup-ibuffer)

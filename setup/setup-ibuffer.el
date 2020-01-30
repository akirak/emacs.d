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
  :hook
  (ibuffer . (defun akirak/ibuffer-vc ()
               (ibuffer-vc-set-filter-groups-by-vc-root)
               (unless (eq ibuffer-sorting-mode 'alphabetic)
                 (ibuffer-do-sort-by-alphabetic)))))

(provide 'setup-ibuffer)

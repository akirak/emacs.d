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
  :config
  (defun ibuffer-project-setup ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
    (unless (eq ibuffer-sorting-mode 'project-file-relative)
      (ibuffer-do-sort-by-project-file-relative)))
  ;; Group buffers by remote connections
  (add-to-list 'ibuffer-project-root-functions
               '(file-remote-p . "Remote"))
  :hook
  (ibuffer . ibuffer-project-setup)
  :custom
  (ibuffer-project-use-cache t)
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " project-file-relative))))

(provide 'setup-ibuffer)

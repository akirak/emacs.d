(use-package projectile
  :init
  (projectile-mode)
  (add-to-list 'projectile-globally-ignored-directories ".cask")
  :custom
  (projectile-completion-system (quote ivy))
  (projectile-create-missing-test-files t)
  (projectile-enable-caching t)
  (projectile-require-project-root nil)
  (projectile-ignored-project-function #'akirak/projectile-ignore-project-p)
  (projectile-keymap-prefix (kbd "C-x 9")))

;;;; Function to determine if a directory is ignored by projectile

(defun akirak/projectile-ignore-project-p (root)
  (or (file-equal-p "~/" root)
      (file-equal-p "~/org/" root)
      (file-equal-p "~/annex/" root)
      (string-prefix-p "/usr/" root)))

(advice-add #'projectile-keep-project-p :after-while
            (lambda (project) (not (akirak/projectile-ignore-project-p project))))

;;;; Delete duplicate projects when the cleanup function is run

(defun akirak/projectile-delete-duplicate-known-projects ()
  (cl-delete-duplicates projectile-known-projects :test #'file-equal-p))

(advice-add 'projectile-cleanup-known-projects :after
            #'akirak/projectile-delete-duplicate-known-projects)

(provide 'setup-projectile)

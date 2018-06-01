(defun akirak/projectile-ignore-project-p (root)
  (or (file-equal-p "~/" root)
      (file-equal-p "~/annex/" root)
      (string-prefix-p "/usr/" root)))

(advice-add #'projectile-keep-project-p :after-while
            (lambda (project) (not (akirak/projectile-ignore-project-p project))))

(defun akirak/projectile-delete-duplicate-known-projects ()
  (cl-delete-duplicates projectile-known-projects :test #'file-equal-p))

(advice-add 'projectile-cleanup-known-projects :after
            #'akirak/projectile-delete-duplicate-known-projects)

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-completion-system (quote ivy))
  (projectile-create-missing-test-files t)
  (projectile-enable-caching t)
  (projectile-require-project-root nil)
  (projectile-ignored-project-function #'akirak/projectile-ignore-project-p)
  (projectile-keymap-prefix (kbd "C-x 9")))

(use-package counsel-projectile
  :after (projectile ivy)
  :init
  (counsel-projectile-mode 1)
  :config
  ;; Use ivy-rich even in counsel-projectile-switch-to-buffer
  (with-eval-after-load 'ivy-rich
    (ivy-set-display-transformer
     'counsel-projectile-switch-to-buffer
     'ivy-rich-switch-buffer-transformer))
  :init
  ;; Open a file or switch to a buffer in the project after projectile-switch-project
  (setq projectile-switch-project-action 'counsel-projectile-find-file))

(provide 'init-projectile)

(defun akirak/projectile-ignore-project-p (root)
  (or (file-equal-p "~" root) ; Ignore the home directory
      (string-prefix-p "/usr/" root)))

(use-package projectile
  :init
  (projectile-mode)
  :custom
  (projectile-completion-system (quote ivy))
  (projectile-create-missing-test-files t)
  (projectile-enable-caching t)
  (projectile-require-project-root nil)
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

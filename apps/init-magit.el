(use-package magit
  :config
  (defun akirak/find-magit-status-buffer (&optional directory)
    (let ((directory (or directory
                         (if (bound-and-true-p projectile-mode)
                             (projectile-project-root)
                           default-directory))))
      (car (member-if (lambda (buf)
                        (string-equal directory
                                      (with-current-buffer buf
                                        default-directory)))
                      (mapcar #'get-buffer
                              (internal-complete-buffer "magit: " nil t))))))
  (defun akirak/magit-status-prefer-existing (&optional directory cache)
    (interactive (list (when current-prefix-arg
                         (read-directory-name "Repository: "))
                       nil))
    (if-let ((buf (akirak/find-magit-status-buffer directory)))
        (progn
          (magit-display-buffer buf)
          (with-current-buffer buf
            (magit-refresh)))
      (magit-status directory cache)))
  :general
  ([remap magit-status] #'akirak/magit-status-prefer-existing)
  :custom
  ;; Automatically save file buffers in the repository
  (magit-save-repository-buffers (quote dontask)))

;;;; Extra sections in =magit-status=
;;;;; magit-todos
;; magit-todos requires hl-todo
(require 'init-hl-todo)
(use-package magit-todos
  :after magit
  :straight (magit-todos :host github :repo "alphapapa/magit-todos")
  :config
  (magit-todos-mode 1))

;;;;; magithub
(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject 'status-checks-header))

(provide 'init-magit)

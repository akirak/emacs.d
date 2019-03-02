(use-package magit
  :config
  (defun akirak/kill-existing-magit-status-buffer ()
    (let* ((directory (if (bound-and-true-p projectile-mode)
                          (projectile-project-root)
                        default-directory))
           (buffer (car (member-if (lambda (buf)
                                     (string-equal directory
                                                   (with-current-buffer buf
                                                     default-directory)))
                                   (mapcar #'get-buffer
                                           (internal-complete-buffer "magit: " nil t))))))
      (when buffer
        (kill-buffer buffer))))
  (advice-add 'magit-status :before 'akirak/kill-existing-magit-status-buffer)
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
  (when (fboundp 'unpackaged/magit-log-date-headers-mode)
    (unpackaged/magit-log-date-headers-mode 1))
  :general
  ("C-x v w" #'magit-worktree
   "C-x M-w" #'magit-worktree-status)
  :custom
  ;; Automatically save file buffers in the repository
  (magit-save-repository-buffers (quote dontask)))

(provide 'setup-magit)

(use-package magit
  :config
  (defun akirak/kill-existing-magit-status-buffers ()
    (let* ((directory (if (bound-and-true-p projectile-mode)
                          (projectile-project-root)
                        default-directory)))
      (dolist (buffer (mapcar #'get-buffer (internal-complete-buffer "magit: " nil t)))
        (when (file-equal-p directory (buffer-local-value 'default-directory buffer))
          (kill-buffer buffer)))))
  (defun akirak/find-magit-status-buffer (&optional directory)
    (let* ((directory (or directory
                          (if (bound-and-true-p projectile-mode)
                              (projectile-project-root)
                            default-directory)))
           (bufs (internal-complete-buffer
                  "magit: "
                  (lambda (cand)
                    (file-equal-p directory
                                  (buffer-local-value 'default-directory
                                                      (cdr cand))))
                  t)))
      (cadr bufs)))
  ;; Based on `unpackaged/magit-status'.
  (defun akirak/magit-status-prefer-existing (directory cache)
    "Enhanced version of `unpackaged/magit-status'.

- Use `akirak/find-magit-status-buffer' to reuse an existing status
  buffer if any.
- With a prefix argument, let the user choose a repository directory.

Open a `magit-status' buffer and close the other window so only Magit is visible.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
    (interactive (list (cond
                        (current-prefix-arg
                         (read-directory-name "Repository: "))
                        (t
                         (locate-dominating-file (or buffer-file-name
                                                     default-directory)
                                                 ".git")))
                       nil))
    (let* ((buffer-file-path (when buffer-file-name
                               ;; Use a directory from the argument.
                               (file-relative-name buffer-file-name directory)))
           (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
      ;; NOTE: This expression was replaced from "(magit-status)" .
      (if-let ((buf (akirak/find-magit-status-buffer directory)))
          (progn
            (magit-display-buffer buf)
            (magit-refresh))
        (magit-status directory cache))
      (delete-other-windows)
      ;; NOTE: Skip if DIRECTORY is explicitly st
      (when (and buffer-file-path
                 (not (string-prefix-p "./" buffer-file-path)))
        (goto-char (point-min))
        (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                         (magit-section-show (magit-current-section))
                         (recenter)
                         t)
                 do (condition-case nil
                        (magit-section-forward)
                      (error (cl-return (magit-status-goto-initial-section-1))))))))
  (when (fboundp 'unpackaged/magit-log-date-headers-mode)
    (unpackaged/magit-log-date-headers-mode 1))
  :general
  ;; C-c M-g is bound to magit-file-dispatch by default.
  ;; I will bind C-c M-KEY to some other frequently used magit commands.
  ("C-S-g" #'magit-dispatch
   "<S-f7>" #'magit-branch-checkout)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  ;; Automatically save file buffers in the repository
  (magit-save-repository-buffers (quote dontask)))

(provide 'setup-magit)

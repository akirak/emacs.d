(defcustom akirak/post-file-rename-functions
  '((emacs-lisp-mode . akirak/post-rename-function/emacs-lisp))
  "Alist of functions applied after a file buffer is renamed.

Each item is a pair of a symbol to a major mode and a function.
The function takes the following arguments:
")

(defvar akirak/rename-file-hook nil
  "Hook run after the current buffer file is renamed.")

(defun akirak/update-buffer-after-renaming (old-file-name)
  (unless (bound-and-true-p projectile-mode)
    (user-error "projectile-mode is not enabled."))
  (when-let* ((func (alist-get major-mode akirak/post-file-rename-functions))
              (new-file-name (buffer-file-name))
              (root (projectile-project-root))
              (relative (and root (file-relative-name new-file-name root))))
    (run-hooks 'akirak/rename-file-hook)
    (when (projectile-file-cached-p old-file-name root)
      (projectile-purge-file-from-cache old-file-name))
    (funcall func old-file-name new-file-name root relative)))

(cl-defun akirak/post-rename-function/emacs-lisp (oldname newname root relative)
  (let ((old-file-name-nondirectory (file-name-nondirectory oldname))
        (old-file-name-base (file-name-base oldname))
        (new-file-name-nondirectory (file-name-nondirectory newname))
        (new-file-name-base (file-name-base newname)))
    (save-excursion
      (goto-char (point-min))
      (query-replace old-file-name-nondirectory new-file-name-nondirectory)
      (goto-char (point-min))
      (query-replace-regexp (concat "^" (regexp-quote
                                         (concat "(provide '" old-file-name-base ")")))
                            (concat "(provide '" new-file-name-base ")")))
    ;; TODO: Rename other files inside the project
    ))

(add-hook 'akirak/rename-file-hook #'projectile-cache-current-file)

(provide 'setup-rename)

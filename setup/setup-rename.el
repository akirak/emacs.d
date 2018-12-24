(defcustom akirak/post-file-rename-functions
  '((emacs-lisp-mode . akirak/post-rename-function/emacs-lisp))
  "Alist of functions applied after a file buffer is renamed.

Each item is a pair of a symbol to a major mode and a function.
The function takes the following arguments:
")

(defun akirak/update-buffer-after-renaming (old-file-name)
  (when-let ((func (alist-get major-mode akirak/post-file-rename-functions)))
    (let* ((new-file-name (buffer-file-name))
           (root (projectile-project-root))
           (relative (and root (file-relative-name new-file-name root))))
      (funcall func old-file-name new-file-name root relative))))

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

(provide 'setup-rename)

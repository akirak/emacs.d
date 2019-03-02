(defun akirak/auto-git-commit-file (file)
  (let* ((file (if file
                   (expand-file-name file)
                 (buffer-file-name)))
         (default-directory (file-name-directory file)))
    (when (magit-inside-worktree-p t)
      (shell-command (apply #'format "git add %s && git commit -m %s %s"
                            (mapcar #'shell-quote-argument
                                    (list file
                                          (format "Auto commit of %s"
                                                  (file-name-nondirectory file))
                                          file)))
                     "*auto-git-commit*"
                     "*auto-git-commit-errors*"))))

(advice-add 'bookmark-write-file :after 'akirak/auto-git-commit-file)

(provide 'setup-git-auto-commit)

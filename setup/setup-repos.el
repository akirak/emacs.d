(use-package git-identity
  :after magit
  :straight (git-identity :host github :repo "akirak/git-identity.el")
  :general
  (:keymaps 'magit-status-mode-map :package 'magit
            "I" #'git-identity-info)

  :config
  (cl-defmacro akirak/git-identity-add (address &rest args)
    (declare (indent 1))
    `(let ((cell (assoc ,address git-identity-list)))
       (if cell
           (setcdr cell (quote ,args))
         (push (cons ,address (quote ,args))
               git-identity-list))))

  :custom
  (git-identity-magit-mode t))

(defun akirak/trash-git-repository (dir &optional arg)
  "Trash a Git repository.

If it succeeds, it returns non-nil."
  (let ((worktrees (let ((default-directory dir))
                     (mapcar #'car (magit-list-worktrees)))))
    (if (not worktrees)
        (when (yes-or-no-p (format-message "%s doesn't look like a worktree. Trash it?"
                                           (abbreviate-file-name dir)))
          (delete-directory dir 'recursive 'trash)
          (message "Deleted %s" dir)
          t)
      (unless arg
        (let ((default-directory dir))
          ;; Only allow removal of worktrees in ~/projects/
          (assert (string-prefix-p "~/projects/"
                                   (abbreviate-file-name default-directory)))
          (assert (not (magit-untracked-files)))
          (assert (not (magit-unstaged-files)))
          (assert (not (magit-staged-files)))
          (assert (not (magit-list-stashes)))
          (assert (not (string-empty-p
                        (magit-git-string "remote" "get-url" "origin"))))
          (dolist (branch (magit-list-local-branch-names))
            (let* ((upstream (magit-get-upstream-branch branch))
                   (ahead1 (and upstream (car (magit-rev-diff-count branch upstream))))
                   (ahead2 (ignore-errors
                             (car (magit-rev-diff-count branch (concat "origin/" branch))))))
              (assert (or (and upstream ahead1 (= 0 ahead1))
                          (and ahead2 (= 0 ahead2)))
                      nil "Branch %s is ahead of %s by %d"
                      branch upstream ahead1)))))
      (when (and (yes-or-no-p (format-message "Trash Git repository %s?"
                                              dir))
                 (or (= 1 (length worktrees))
                     (yes-or-no-p (concat "There are other worktrees. Remove them too?\n"
                                          (string-join
                                           (cl-remove dir worktrees :test #'file-equal-p)
                                           "\n")))
                     (user-error "Aborted")))
        (dolist (worktree worktrees)
          (delete-directory worktree 'recursive 'trash))
        (message "Deleted %s" (string-join worktrees " "))
        t))))

(provide 'setup-repos)

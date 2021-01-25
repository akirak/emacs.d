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
          (assert (cl-member (abbreviate-file-name default-directory)
                             '("~/projects/" "~/work/")
                             :test (-flip #'string-prefix-p)))
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

(cl-defun akirak/git-commit-date (&optional (rev "HEAD"))
  (with-temp-buffer
    (insert (call-process-with-args "git" "cat-file" "-p" rev))
    (goto-char (point-min))
    (re-search-forward (rx bol "committer "))
    (let ((str (thing-at-point 'line t)))
      (string-match (rx (group (+ digit)) (+ space)
                        (group (any "-+"))
                        (group (repeat 2 (any digit)))
                        (group (repeat 2 (any digit)))
                        eol)
                    str)
      (let ((secs (cl-parse-integer (match-string 1 str)))
            (sign (match-string 2 str))
            (hours (cl-parse-integer (match-string 3 str)))
            (minutes (cl-parse-integer (match-string 4 str))))
        (->> (+ (* 3600 hours) (* 60 minutes))
             (funcall (if (string= sign "+") #'+ #'-))
             (+ secs)
             (decode-time)
             (encode-time))))))

(defun akirak/git-archive-repository ()
  "Save the latest commit of this repository to my repository archive.

This is used to keep protptype code for later use."
  (interactive)
  ;; TODO: Maybe verify the origin URL to prevent myself from
  ;; accidentally importing code not written by me
  (let* ((name (->> (or (magit-toplevel)
                        (user-error "No top level"))
                    (f-filename)))
         (dest (f-join commonplace-root "archive"
                       (format-time-string "%Y%m" (akirak/git-commit-date))
                       name))
         (tmpfile (make-temp-file "git-archive" nil ".tar")))
    (unwind-protect
        (progn
          (when (file-directory-p dest)
            (user-error "Directory %s already exists" dest))
          (call-process-with-args "git" "archive" "-o" tmpfile "HEAD")
          (make-directory dest t)
          (call-process-with-args "tar" "-C" (expand-file-name dest) "-x" "-f" tmpfile)
          (message "Finished adding to %s" dest))
      (delete-file tmpfile))))

(provide 'setup-repos)

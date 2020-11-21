(use-package magit-section)

(use-package magit
  :config
  (when-let ((bin (executable-find "git")))
    (setq magit-git-executable bin))
  (when (fboundp 'unpackaged/magit-log-date-headers-mode)
    (unpackaged/magit-log-date-headers-mode 1))

  ;; It seems that magit fails to check out a branch containing
  ;; non-latin characters. This is caused by mistakenly encoding
  ;; command line arguments passed to git. The cdr of
  ;; default-process-coding-system is iso-latin-1-unix, but I will use
  ;; utf-8 instead as a workaround. If this causes trouble, I will
  ;; look for an alternative solution.
  (advice-add 'magit--process-coding-system :override
              (lambda () '(utf-8-unix . utf-8-unix)))

  ;; Functions for magit-list-repositories.
  (defun akirak/magit-repolist-column-group (_id)
    (f-filename (abbreviate-file-name (f-parent default-directory))))

  (defun akirak/magit-repolist-column-path (_id)
    (string-join `(,@(--map (cond
                             ((string-empty-p it)
                              "")
                             ((string-prefix-p "." it)
                              (substring it 1 2))
                             (t
                              (substring it 0 1)))
                            (split-string (f-short (f-parent default-directory)) "/"))
                   ,(f-filename default-directory))
                 "/"))

  (defun akirak/magit-repolist-column-commit-date (_id)
    "Insert a description of the repository's `HEAD' revision."
    (let ((v (or (magit-git-string "describe" "--tags")
                 ;; If there are no tags, use the date in MELPA format.
                 (magit-git-string "show" "--no-patch" "--format=%cd"
                                   "--date=format:%Y-%m-%d"))))
      (if (and v (string-match-p "\\`[0-9]" v))
          v
        (concat " " v))))

  (defun akirak/magit-repolist-column-unmerged (id)
    (let ((lines (magit-git-lines "show-unmerged-branches"
                                  (magit-repolist-column-branch id))))
      (string-join lines ",")))

  (defun akirak/magit-repolist-column-origin (_id)
    (string-trim-left (or (magit-git-string "remote" "get-url" "origin")
                          "")
                      (rx (or "https://" "git@" "git://"))))

  (defun akirak/magit-repolist-column-dirty (_id)
    "Insert a letter if there are uncommitted changes.

Show N if there is at least one untracked file.
Show U if there is at least one unstaged file.
Show S if there is at least one staged file.
Only one letter is shown, the first that applies."
    (cond ((magit-untracked-files) "?")
          ((magit-unstaged-files)  "*")
          ((magit-staged-files)    "+")))

  (defun akirak/magit-modulelist-column-path (path)
    (let* ((segs (f-split path)))
      (apply #'f-join `(,@(--map (seq-take it 1) (-butlast segs))
                        ,(-last-item segs)))))

  (general-def :package 'magit-repolist :keymaps 'magit-repolist-mode-map
    "k" (defun akirak/magit-repolist-kill-origin-url-at-point ()
          (interactive)
          (let* ((default-directory (tabulated-list-get-id))
                 (url (magit-git-string "remote" "get-url" "origin")))
            (kill-new url)
            (message "Saved to kill ring: %s" url)))

    "D" (defun akirak/magit-repolist-trash-repository-at-point (&optional arg)
          (interactive "P")
          (let* ((dir (tabulated-list-get-id))
                 (worktrees (let ((default-directory dir))
                              (mapcar #'car (magit-list-worktrees)))))
            (if (not worktrees)
                (when (yes-or-no-p (format-message "%s doesn't look like a worktree. Trash it?"
                                                   (abbreviate-file-name dir)))
                  (delete-directory dir 'recursive 'trash)
                  (tabulated-list-delete-entry)
                  (message "Deleted %s" dir))
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
                (tabulated-list-delete-entry)
                (message "Deleted %s" (string-join worktrees " ")))))))

  (akirak/bind-f8
    ;; <f8> <f8>
    "<f8>" #'magit-status
    ;; <f8> <f7>
    "<f7>" #'magit-stage-file
    ;; <f8> <f9>
    "<f9>" #'magit-dispatch
    ;; <f8> <f10>
    "<f10>" #'magit-file-dispatch)
  (akirak/bind-file-extra
    "g" #'magit-blob-visit-file
    "l" #'magit-log-buffer-file
    "s" #'magit-stage-file)

  ;; Sort the repo list by date in descending order
  (add-hook 'magit-repolist-mode-hook
            (defun akirak/magit-repolist-sort-by-date ()
              (cl-dotimes (i 2)
                (tabulated-list-sort 7))))

  (add-hook 'magit-credential-hook 'akirak/ensure-gpg-ssh-auth-sock)

  :custom
  (magit-repository-directories
   '(("~/.emacs.d" . 0)
     ;; ("~/.emacs.d/straight/repos/" . 1)
     ("~" . 1)
     ("~/.config" . 1)
     ("~/projects" . 2)
     ("~/lib" . 1)
     ("/etc/nixos" . 0)))
  (magit-repolist-columns
   '(("Path" 30 akirak/magit-repolist-column-path nil)
     ("Branch" 20 magit-repolist-column-branch nil)
     ("Drty" 4 akirak/magit-repolist-column-dirty nil)
     ("Unmg" 5 akirak/magit-repolist-column-unmerged nil)
     ("Stsh" 4 magit-repolist-column-stashes nil)
     ("B<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)
       (:help-echo "Upstream changes not in branch")))
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)
       (:help-echo "Local changes not in upstream")))
     ("Date" 12 akirak/magit-repolist-column-commit-date nil)
     ("origin" 30 akirak/magit-repolist-column-origin nil)))
  (magit-submodule-list-columns
   '(("Path" 25 akirak/magit-modulelist-column-path nil)
     ("Version" 13 magit-repolist-column-version nil)
     ("Branch" 9 magit-repolist-column-branch nil)
     ("Drty" 4 akirak/magit-repolist-column-dirty nil)
     ("B<U" 3 magit-repolist-column-unpulled-from-upstream
      ((:right-align t)))
     ("B>U" 3 magit-repolist-column-unpushed-to-upstream
      ((:right-align t)))
     ("B<P" 3 magit-repolist-column-unpulled-from-pushremote
      ((:right-align t)))
     ("B>P" 3 magit-repolist-column-unpushed-to-pushremote
      ((:right-align t)))
     ("B" 3 magit-repolist-column-branches
      ((:right-align t)))
     ("S" 3 magit-repolist-column-stashes
      ((:right-align t)))
     ("origin" 35 akirak/magit-repolist-column-origin nil)))
  (magit-display-buffer-function
   'magit-display-buffer-same-window-except-diff-v1)
  ;; Don't use `magit-file-mode-map'
  (global-magit-file-mode nil)
  ;; Automatically save file buffers in the repository
  (magit-save-repository-buffers (quote dontask)))

(use-package orgit
  :after org)

(use-package magit-annex
  :after magit)

(provide 'setup-magit)

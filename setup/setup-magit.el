(use-package magit-section)

(defcustom akirak/magit-large-repositories nil
  "List of large Git repositories where Magit should limit its features."
  :type '(repeat directory))

(use-package magit
  :init
  (setq with-editor-emacsclient-executable
        (executable-find "emacsclient"))
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

  (add-hook 'magit-process-mode-hook #'compilation-minor-mode)

  ;; Speed up Magit on large Git repositories by limiting the types of
  ;; information displayed inside `magit-status' buffer.
  ;;
  ;; Based on ideas from this article:
  ;; https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
  (general-advice-add '(
                        ;; I don't want to remove these headers right now
                        ;;
                        ;; magit-insert-tags-header
                        ;; magit-insert-status-headers
                        magit-insert-unpushed-to-pushremote
                        magit-insert-unpulled-from-pushremote
                        magit-insert-unpulled-from-upstream
                        magit-insert-unpushed-to-upstream-or-recent)
                      :before-while
                      (defun akirak/magit-not-inside-large-repository-p ()
                        (not (cl-member (magit-toplevel)
                                        akirak/magit-large-repositories
                                        :test #'file-equal-p))))

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
          (when (akirak/trash-git-repository (tabulated-list-get-id) arg)
            (tabulated-list-delete-entry)))

    "R" (defun akirak/magit-repolist-rename-repository-at-point ()
          (interactive)
          (let* ((dir (tabulated-list-get-id))
                 (worktrees (let ((default-directory dir))
                              (mapcar #'car (magit-list-worktrees)))))
            (if (> (length worktrees) 1)
                (user-error "You can't rename the repository if it has other working trees.")
              (let ((new-name (read-directory-name "New name: ")))
                (when (file-exists-p new-name)
                  (user-error "File/directory %s already exists" new-name))
                (rename-file dir new-name))))))

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
     ("~/.config" . 1)
     ;; Deprecated
     ("~/projects" . 2)
     ;; domain/org - group - worktree
     ("~/work" . 3)
     ("~/lib" . 1)
     ("~" . 1)
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

(use-package magit-delta
  ;; I don't always need the features of magit-delta-mode, so I will
  ;; turn it on only in certain repositories.
  ;;
  ;; Add it to .dir-locals.el in repositories where you need it.
  ;;
  ;; :config
  ;; (add-hook 'magit-mode-hook #'magit-delta-mode)
  :after magit)

(use-package orgit
  :after org)

(use-package magit-annex
  :after magit)

(provide 'setup-magit)

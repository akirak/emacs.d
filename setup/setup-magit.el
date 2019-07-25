(use-package magit
  :config
  (when-let ((bin (executable-find "git")))
    (setq magit-git-executable bin))
  (when (fboundp 'unpackaged/magit-log-date-headers-mode)
    (unpackaged/magit-log-date-headers-mode 1))
  ;; Functions for magit-list-repositories.
  (defun akirak/magit-repolist-column-group (_id)
    (f-filename (abbreviate-file-name (f-parent default-directory))))

  (defun akirak/magit-repolist-column-path (_id)
    (string-join `(,@(--map (if (string-prefix-p "." it)
                                (substring it 0 1)
                              (substring it 0 1))
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
  :general
  ;; C-c M-g is bound to magit-file-dispatch by default.
  ;; I will bind C-c M-KEY to some other frequently used magit commands.
  ("C-S-g" #'magit-dispatch)
  :custom
  (magit-repolist-columns
   '(("Path" 20 akirak/magit-repolist-column-path nil)
     ("Branch" 20 magit-repolist-column-branch nil)
     ("Drty" 4 magit-repolist-column-dirty nil)
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
  (magit-display-buffer-function
   (if akirak/to-be-run-as-exwm
       'magit-display-buffer-same-window-except-diff-v1
     'magit-display-buffer-fullframe-status-v1))
  ;; Automatically save file buffers in the repository
  (magit-save-repository-buffers (quote dontask)))

(provide 'setup-magit)

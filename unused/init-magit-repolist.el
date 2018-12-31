(defcustom akirak/magit-repolist-prefix-path nil
  "When non-nil, prefix paths with group numbers for sorting."
  :type 'boolean
  :group 'akirak-magit-repolist)

(defun akirak/magit-repolist-column-group (id)
  (let ((parent (file-name-directory default-directory)))
    (cond
     ((file-equal-p parent (expand-file-name "~"))
      "home")
     ((string-match-p abbreviated-home-dir (expand-file-name parent))
      (string-remove-suffix "/" (file-relative-name parent "~")))
     (t parent))))

(defun akirak/magit-repolist-column-path-prefix (id)
  (pcase (akirak/magit-repolist-column-group id)
    ("home" "1:")
    ("github" "2:")
    (_ "3:")))

(defun akirak/magit-repolist-column-path (id)
  (concat (if akirak/magit-repolist-prefix-path
              (akirak/magit-repolist-column-path-prefix id)
            "")
          (abbreviate-file-name default-directory)))

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
  ;; TODO: Shorten the URL
  (magit-git-string "remote" "get-url" "origin"))

;; TODO: Colorize
(setq magit-repolist-columns
      '(("Group" 8 akirak/magit-repolist-column-group nil)
        ("Name" 25 magit-repolist-column-ident nil)
        ("Branch" 13 magit-repolist-column-branch nil)
        ("Date" 10 akirak/magit-repolist-column-commit-date nil)
        ;; TODO: Translate the format for easier comprehension
        ("Dirty" 5 magit-repolist-column-dirty nil)
        ("Stash" 5 magit-repolist-column-stashes nil)
        ("Unmerged" 13 akirak/magit-repolist-column-unmerged nil)
        ("B<U" 3 magit-repolist-column-unpulled-from-upstream
         ((:right-align t)
          (:help-echo "Upstream changes not in branch")))
        ("B>U" 3 magit-repolist-column-unpushed-to-upstream
         ((:right-align t)
          (:help-echo "Local changes not in upstream")))
        ("origin" 50 akirak/magit-repolist-column-origin nil)
        ("Path" 10 akirak/magit-repolist-column-path nil)))

(provide 'init-magit-repolist)

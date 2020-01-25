(use-package ivy-omni-org
  :custom
  (ivy-omni-org-bookmark-display-transformer nil)
  (ivy-omni-org-file-sources '(org-starter-known-files
                               akirak/conflict-org-files))
  (ivy-omni-org-content-types '(agenda-commands
                                org-ql-views
                                buffers
                                files
                                bookmarks)))

(defun akirak/conflict-org-files ()
  "List of Syncthing conflict files in `org-directory'.g"
  (when (and (bound-and-true-p org-directory)
             (file-directory-p org-directory))
    (directory-files org-directory t "\\.sync-conflict-[-[:digit:]]+\\.org\\'")))

(akirak/bind-register-map
  "M-b" #'ivy-omni-org-bookmarks
  "M-o" #'ivy-omni-org)

(provide 'setup-ivy-omni-org)

(org-starter-def "~/Dropbox/org"
  :add-to-path t
  :ensure nil
  :config
  (org-starter-define-file "scratch.org"
    :key "i"
    :required nil
    :custom-vars 'org-default-notes-file
    :agenda t
    :refile '(:maxlevel . 2))
  (org-starter-define-file "tasks.org"
    :required nil
    :agenda t
    :refile '(:maxlevel . 9))
  (setq bookmark-default-file "~/Dropbox/emacs/bookmarks"
        akirak/init-time-log-file "~/Dropbox/emacs/init.log"
        org-download-screenshot-file "~/Dropbox/Screenshots/scrot.png"
        org-offtime-file "~/Dropbox/emacs/offtime.org"
        akirak/org-agenda-view-directory "~/Dropbox/org/agenda")
  (provide 'my-dropbox))

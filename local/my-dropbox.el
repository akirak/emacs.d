(org-starter-def "~/Dropbox/org"
  :ensure nil
  :config
  (setq bookmark-default-file "~/Dropbox/emacs/bookmarks"
        akirak/init-time-log-file "~/Dropbox/emacs/init.log"
        org-download-screenshot-file "~/Dropbox/Screenshots/scrot.png"
        org-offtime-file "~/Dropbox/emacs/offtime.org"
        akirak/org-agenda-view-directory "~/Dropbox/org/agenda")
  (provide 'my-dropbox))

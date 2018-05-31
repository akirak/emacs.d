(define-prefix-command 'akirak/global-org-map)

(general-def akirak/global-org-map
  "j" #'org-journal-new-entry
  "nj" #'org-journal-new-date-entry
  "r" #'helm-org-rifle-agenda-files
  "f" #'helm-org-starter
  "sj" #'org-journal-search)

(provide 'init-global-org-map)

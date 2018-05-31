(require 'init-helm)

(use-package helm-org-rifle
  :after (helm org)
  :custom
  (helm-org-rifle-show-path t))

;;;; helm-org-rifle + org-recent-headings

(require 'init-org-recent-headings)
;; TODO: Defer loading
(require 'helm-org-rifle)
(helm-org-rifle-define-command
 "agenda-files-with-recent-headings" ()
 "Rifile with org-recent-headings prepended."
 :sources (cons helm-source-org-recent-headings
                (mapcar 'helm-org-rifle-get-source-for-file (org-agenda-files))))

(provide 'init-helm-org-rifle)

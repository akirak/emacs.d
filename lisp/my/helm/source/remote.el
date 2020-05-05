(require 'my/remote/enum)

(defconst akirak/helm-source-recent-remotes
  (helm-make-source "Recent remotes" 'helm-source-sync
    :candidates #'akirak/recent-remote-identifiers
    :action
    '(("Recent files"
       . (lambda (remote)
           (helm (helm-build-sync-source "Recent files"
                   :candidates
                   (->> recentf-list
                        (-filter (-partial #'string-prefix-p remote)))
                   :action #'find-file))))
      ("Find file" . (lambda (remote)
                       (let ((default-directory remote))
                         (counsel-find-file))))
      ("Dired" . #'dired)
      ("Eshell" . (lambda (remote)
                    (let ((default-directory remote))
                      (eshell)))))))

(provide 'my/helm/source/remote)

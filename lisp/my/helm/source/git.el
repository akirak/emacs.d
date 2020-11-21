(defconst akirak/helm-remote-repo-dummy-source
  (helm-make-source "Remote Git repository" 'helm-source-dummy
    ;; TODO: Add meaningful actions
    :action #'message))

(provide 'my/helm/source/git)

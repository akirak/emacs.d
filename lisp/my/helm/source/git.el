(require 'my/helm/action/giturl)

(defconst akirak/helm-remote-repo-dummy-source
  (helm-make-source "Remote Git repository" 'helm-source-dummy
    :action 'akirak/helm-git-url-dummy-action))

(provide 'my/helm/source/git)
